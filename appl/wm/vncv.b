# vnc, or "remote frame buffer", rfc 6143
#
# connecting is done by new thread connect().
# when connected:
# - thread fb() receives and applies framebuffer changes
# - thread vncread() reads remote events.  most are fb updates, sent to fb().
# - thread vncwrite() writes events/requests to remote.
# - init() receives tk, connection & vncread() events, send commands to vncwrite() and updates to fb().

implement Vncv;

include "sys.m";
	sys: Sys;
	sprint: import sys;
include "draw.m";
	draw: Draw;
	Chans, Display, Rect, Point, Image, Pointer: import draw;
include "arg.m";
include "bufio.m";
	bufio: Bufio;
	Iobuf: import bufio;
include "convcs.m";
	convcs: Convcs;
	tolatin1: Stob;
	fromlatin1: Btos;
include "dial.m";
	dial: Dial;
include "filter.m";
	inflate: Filter;
include "keyboard.m";
	kb: Keyboard;
include "keyring.m";
	kr: Keyring;
include "string.m";
	str: String;
include "tk.m";
	tk: Tk;
include "tkclient.m";
	tkclient: Tkclient;

Vncv: module {
	init:	fn(ctxt: ref Draw->Context, argv: list of string);
};


dflag: int;
lflag: int;
Sflag: int;
password: string;
encodings: list of int;
addr: string;
pixelformat: ref Pixelformat;

Pixelformat: adt {
	bitpp:		int;
	depth:		int;
	bigendian:	int;
	truecolor:	int;
	max:		(int, int, int);
	shift:		(int, int, int);
	unshift:	(int, int, int);  # 8-log2(max)
	cpx3:		int;  # compressed pixel in 3 bytes

	text:		fn(p: self Pixelformat): string;
};

Serverinit: adt {
	width:	int;
	height:	int;
	pf:	Pixelformat;
	name:	string;
};

Pixelformat.text(p: self Pixelformat): string
{
	return sprint("bitpp=%d depth=%d bigendian=%d truecolor=%d max=(%d,%d,%d) shift=(%d,%d,%d) unshift=(%d,%d,%d) cpx3=%d",
		p.bitpp, p.depth, p.bigendian, p.truecolor,
		p.max.t0, p.max.t1, p.max.t2,
		p.shift.t0, p.shift.t1, p.shift.t2,
		p.unshift.t0, p.unshift.t1, p.unshift.t2,
		p.cpx3);
}

Z: adt {
	pid:	int;
	rqc:	chan of ref Filter->Rq;
	f:	ref Filter->Rq.Fill;
};

Link: adt[T] {
	e:	T;
	next:	cyclic ref Link[T];
};

List: adt[T] {
	first,
	last:	ref Link[T];

	empty:		fn(l: self ref List[T]): int;
	append:		fn(l: self ref List[T], v: T);
	takefirst:	fn(l: self ref List[T]): T;
};

List[T].empty(l: self ref List[T]): int
{
	return l.first == nil;
}

List[T].append(l: self ref List[T], v: T)
{
	n := ref Link[T](v, nil);
	if(l.first == nil)
		l.first = l.last = n;
	else {
		l.last.next = n;
		l.last = n;
	}
}

List[T].takefirst(l: self ref List[T]): T
{
	r := l.first.e;
	if(l.first == l.last)
		l.first = l.last = nil;
	else
		l.first = l.first.next;
	return r;
}

Req: adt {
	pick {
	Fbup =>
		incr:	int;
		r:	Rect;
	Ptr =>
		p:	Pointer;
	Snarf =>
		s:	string;
	Key =>
		ctl:	int;
		c:	int;
	}
};

Conn: adt {
	i:		ref Iobuf;
	o:		ref Iobuf;
	read:		string;		# currently reading, for error messages
	write:		string;
	incr:		int;		# request incremental fb update?
	si:		Serverinit;
	beep:		int;		# where to draw beep msg
	fbr:		Rect;
	fborig:		Point;		# screen coord of top-left corner of panel with framebuffer image, for ptr events
	ptrvnc:		int;		# last mouse was on our tk vnc image?
	ptr:		Point;		# last ptr on fb, relative to fborig
	ptrimg:		ref Image;	# current image of cursor, for local cursor drawing
	ptrhotspot:	Point;		# offset of mouse in ptrimg
	ptrbackup:	ref Image;	# image under current ptr
	ptrbackuporig:	Point;
	pids:		list of int;
	zz:		ref Z;

	# write to remote
	outc:		chan of ref Req;
	busy:		int;
	out:		ref List[ref Req];

	# actions on framebuffer, executed by fb()
	drawing:	int;	# whether currently drawing (and ptr should not be drawn)
	nfbc:		chan of ref Image;
	fbgetc:		chan of chan of ref Image;
	fbpxc:		chan of (Rect, array of byte, int);
	fbdrawc:	chan of (Rect, ref Image, Point, int);
	fbselfdrawc:	chan of (Rect, Point, int);
	fbfbdrawc:	chan of (ref Image, Rect, Point);
	fbflushc:	chan of Rect;
};

connpid := -1;

connc: chan of (ref Conn, string);
wrotec: chan of int;
unptrc: chan of chan of int;
cursorc: chan of (ref Image, Point);
resizec: chan of (int, int);
passc: chan of chan of string;

disp: ref Display;
top: ref Tk->Toplevel;
wmctl: chan of string;

ZP: con Point(0,0);


# authentication types
Abad, Anone, Avncauth: con iota;

# client to server
Cpxfmt: con 0;
Cenc, Cfbup, Ckey, Cptr, Ctext: con 2+iota;

# server to client
Sfbup, Scolmap, Sbell, Stext: con iota;

# framebuffer update encodings
Eraw:		con 0;
Ecopyrect:	con 1;
Erre:		con 2;
Ehextile:	con 5;
Etrle:		con 15;
Ezrle:		con 16;
Ecursor:	con -239;
Eresize:	con -223;

# Etrle is not implemented because i cannot test it:  realvnc & tightvnc don't seem to serve it
# Ezrle does not work, possibly due to inflate
encstrs := array[] of {
(Eraw,		"raw"),
(Ecopyrect,	"copyrect"),
(Erre,		"rre"),
(Ehextile,	"hextile"),
#(Etrle,	"trle"),
(Ezrle,		"zrle"),
(Ecursor,	"localcursor"),
(Eresize,	"resize"),
};

init(ctxt: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	if(ctxt == nil)
		fail("no window context");
	disp = ctxt.display;
	draw = load Draw Draw->PATH;
	arg := load Arg Arg->PATH;
	bufio = load Bufio Bufio->PATH;
	dial = load Dial Dial->PATH;
	inflate = load Filter Filter->INFLATEPATH;
	inflate->init();
	kr = load Keyring Keyring->PATH;
	str = load String String->PATH;
	tk = load Tk Tk->PATH;
	tkclient = load Tkclient Tkclient->PATH;

	convcs = load Convcs Convcs->PATH;
	err := convcs->init(nil);
	if(err != nil)
		fail("convcs: "+err);
	(tolatin1, err) = convcs->getstob("latin1");
	if(err != nil)
		fail("utf-8 to latin1 converter: "+err);
	(fromlatin1, err) = convcs->getbtos("latin1");
	if(err != nil)
		fail("latin1 to utf-8 converter: "+err);

	# in reverse order
	encodings = list of {Ecopyrect, Eraw, Ehextile, Erre, Eresize, Ecursor};

	encreset := 0;
	arg->init(args);
	arg->setusage(arg->progname()+" [-d] [-e encoding] [-w 8|16|24] [-p [l]RGBNNN] [-S] [-l [addr] | addr]");
	while((c := arg->opt()) != 0)
		case c {
		'd' =>	dflag++;
		'e' =>
			(ok, e) := enclookup(arg->earg());
			if(!ok) {
				warn("bad encoding");
				arg->usage();
			}
			if(!encreset) {
				encreset = 1;
				encodings = nil;
			}
			encodings = e::encodings;
		'w' =>
			case int arg->earg() {
			8 =>	pixelformat = parsepxfmt("lbgr233");
			16 =>	pixelformat = parsepxfmt("lbgr556");
			24 =>	pixelformat = parsepxfmt("lrgb888");
			* =>	arg->usage();
			}
			if(pixelformat == nil)
				arg->usage();
		'p' =>
			pixelformat = parsepxfmt(arg->earg());
			if(pixelformat == nil)
				arg->usage();
		'l' =>	lflag++;
		'S' =>	Sflag++;
		* =>	arg->usage();
		}
	args = arg->argv();
	if(lflag && args == nil)
		args = "*"::nil;
	if(len args != 1)
		arg->usage();
	encodings = revint(encodings);

	sys->pctl(Sys->NEWPGRP, nil);
	tkclient->init();
	if(lflag)
		title := sprint("vncv listen %s", addr);
	else
		title = "vncv "+hd args;
	(top, wmctl) = tkclient->toplevel(ctxt, "", title, Tkclient->Appl);

	addr = dial->netmkaddr(hd args, "net", "5900");
	if(lflag)
		addr = dial->netmkaddr(hd args, "tcp", "5500");

	cmdc := chan of string;
	tk->namechan(top, cmdc, "cmd");
	tkcmd("frame .c");
	tkcmd("button .c.refresh -text refresh -command {send cmd refresh}");
	tkcmd("button .c.send -text {send snarf} -command {send cmd send}");
	tkcmd("button .c.connect -text connect -command {send cmd connection}");
	tkcmd("button .c.debug -text debug -command {send cmd debug}");
	if(dflag)
		tkcmd(".c.debug configure -text nodebug");
	tkcmd("label .c.msg");
	tkcmd("entry .c.pass -width 10w -show *");
	tkcmd("bind .c.pass {<Key-\n>} {send cmd pass}");
	tkcmd("panel .p");
	tkcmd("pack .c.refresh .c.send .c.debug .c.connect .c.msg -side left -anchor w");
	tkcmd("pack .c -expand 1 -fill x");
	tkcmd("pack .p");

	tkclient->onscreen(top, nil);
	tkclient->startinput(top, "kbd"::"ptr"::nil);

	connc = chan of (ref Conn, string);
	wrotec = chan of int;
	unptrc = chan of chan of int;
	cursorc = chan of (ref Image, Point);
	resizec = chan of (int, int);
	passc = chan of chan of string;
	passrc: chan of string;
	cc: ref Conn;

	spawn connect(cpidc := chan of int);
	connpid = <-cpidc;

	fbrqc := chan of int;
	for(;;)
	alt {
	pwrc := <-passc =>
		tkmsg("password:");
		tkcmd("pack .c.pass -after .c.msg");
		tkcmd("focus .c.pass");
		tkcmd("update");
		passrc = pwrc;

	(nc, m) := <-connc =>
		nfb: ref Image;
		if(m == nil) {
			say(sprint("have connection, size %d.%d", nc.si.width,nc.si.height));
			say("using pixelformat "+nc.si.pf.text());
			nfb = disp.newimage(Rect(ZP,(nc.si.width,nc.si.height)), draw->RGB24, 0, draw->Black);
			if(nfb == nil) {
				m = sprint("image: %r");
			} else {
				m = tk->putimage(top, ".p", nfb, nil);
				if(m != nil)
					m = "putimage: "+m;
			}
		}
		tkmsg(m);
		if(m != nil) {
			tkdisconnect(nil);
			passrc = nil;
			cc = nil;
		} else {
			cc = nc;
			tkmsg("connected to "+cc.si.name);

			tkgetorigin(cc);
			spawn vncread(cc, fbrqc, pidc := chan of int);
			spawn vncwrite(cc, pidc);
			spawn fb(cc, nfb, pidc);
			cc.pids = list of {<-pidc, <-pidc, <-pidc};
		}
		tkcmd("update");

	rc := <-unptrc =>
		ptrrestore(cc);
		cc.drawing = 1;
		rc <-= 1;

	<-fbrqc =>
		r := Rect(ZP, (cc.si.width, cc.si.height));
		put(cc, ref Req.Fbup(cc.incr, r));
		cc.incr = 1;
		cc.drawing = 0;
		ptrdraw(cc);

	(img, hot) := <-cursorc =>
		cc.ptrimg = img;
		cc.ptrhotspot = hot;

	(w, h) := <-resizec =>
		say(sprint("new remote size %d.%d", w, h));
		nfb := disp.newimage(Rect(ZP,(w,h)), draw->RGB24, 0, draw->Black);
		rr := Rect(ZP,(w,h));
		cc.fbgetc <-= rc := chan of ref Image;
		nfb.draw(rr, <-rc, nil, ZP);
		nfb.flush(draw->Flushnow);
		tk->putimage(top, ".p", nfb, nil);
		tkcmd("update");
		cc.nfbc <-= nfb;
		tkgetorigin(cc);

		cc.si.width = w;
		cc.si.height = h;
		cc.fbr = Rect((0,0), (w,h));

	k := <-top.ctxt.kbd =>
		if(cc != nil && cc.ptrvnc)
			key(cc, k);
		else
			tk->keyboard(top, k);

	s := <-top.ctxt.ptr =>
		tk->pointer(top, *s);
		if(cc == nil)
			continue;
		(x, y) := (s.xy.x, s.xy.y);
		(ox, oy) := (cc.fborig.x, cc.fborig.y);
		nptrvnc := x >= ox && y >= oy && (nx := x-ox) < cc.si.width && (ny := y-oy) < cc.si.height;
		ptrrestore(cc);
		if(nptrvnc) {
			cc.ptr = Point(nx, ny);
			ptrdraw(cc);
			p := *s;
			p.xy = cc.ptr;
			put(cc, ref Req.Ptr(p));
		}
		if(cc != nil)
			cc.ptrvnc = nptrvnc;

	<-wrotec =>
		cc.busy = 0;
		write(cc);

	s := <-top.ctxt.ctl or
	s = <-top.wreq or
	s = <-wmctl =>
		tkclient->wmctl(top, s);
		if(cc != nil)
			tkgetorigin(cc);

	s := <-cmdc =>
		case s {
		"debug" =>
			dflag = !dflag;
			t := "debug";
			if(dflag)
				t = "no"+t;
			tkcmd(".c.debug configure -text "+t);
		"pass" =>
			passrc <-= tkcmd(".c.pass get 0 end");
			tkcmd(".c.pass delete 0 end");
			tkcmd("pack forget .c.pass");
			passrc = nil;
		"refresh" =>
			cc.incr = 0;
		"send" =>
			put(cc, ref Req.Snarf(tkclient->snarfget()));
		"connection" =>
			if(cc != nil || connpid >= 0) {
				tkmsg("");
				tkdisconnect(cc);
				passrc = nil;
				cc = nil;
			} else {
				spawn connect(pidc := chan of int);
				connpid = <-pidc;
			}
		* =>
			warn(sprint("unknown command %q", s));
		}
		tkcmd("update");
	}
}

parsepxfmt(s: string): ref Pixelformat
{
	bigendian := 1;
	if(str->prefix("l", s)) {
		bigendian = 0;
		s = s[1:];
	}
	if(len s != 6)
		return nil;
	ri := gi := bi := -1;
	for(i := 0; i < 3; i++)
		case s[i] {
		'r' =>	ri = i;
		'g' =>	gi = i;
		'b' =>	bi = i;
		* =>	return nil;
		}
	if(ri < 0 || gi < 0 || bi < 0)
		return nil;
	widths := array[3] of int;
	depth := 0;
	for(i = 3; i < 6; i++) {
		d := s[i]-'0';
		if(d < 0 || d > 8)
			return nil;
		widths[i-3] = d;
		depth += d;
	}
	bitpp: int;
	case depth {
	8 or 16 =>
		bitpp = depth;
	24 =>
		bitpp = 32;
	* =>
		warn("bits must sum to 8, 16 or 24");
		return nil;
	}
	max := ((1<<widths[ri])-1, (1<<widths[gi])-1, (1<<widths[bi])-1);
	shifts := array[] of {widths[1]+widths[2], widths[2], 0};
	shift := (shifts[ri], shifts[gi], shifts[bi]);
	unshift := (8-widths[ri], 8-widths[gi], 8-widths[bi]);
	cpx3 := bitpp==32 && depth<=24;
	pxfmt := ref Pixelformat(bitpp, depth, bigendian, 1, max, shift, unshift, cpx3);
	say("configured pixelformat "+(*pxfmt).text());
	return pxfmt;
}

key(cc: ref Conn, c: int)
{
	# xxx see if we can get shift/control/meta/alt/function keys from tk
	if(dflag) say(sprint("key %d/%#x", c, c));
	Ctl: con 16r40;
	isctl := 0;
	case c {
	'H'-Ctl =>	c = 16rff08;
	'\t' =>		c = 16rff09;
	'\n' =>		c = 16rff0d;
	kb->Esc =>	c = 16rff1b;
	kb->Ins =>	c = 16rff63;
	kb->Del =>	c = 16rffff;
	kb->Home =>	c = 16rff50;
	kb->End =>	c = 16rff57;
	kb->Pgup =>	c = 16rff55;
	kb->Pgdown =>	c = 16rff56;
	kb->Left =>	c = 16rff51;
	kb->Right =>	c = 16rff53;
	kb->Down =>	c = 16rff54;
	* =>
		if(c > 0 && c < 16r20) {
			isctl = 1;
			c += Ctl;
		} else if(c >= kb->KF && c < kb->KF+12) {
			# xxx we don't get these from tk...
			c -= kb->KF;
			c += 16rffbe;
		}
	}
	put(cc, ref Req.Key(isctl, c));
}

write(cc: ref Conn)
{
	if(!cc.busy && !cc.out.empty()) {
		cc.outc <-= cc.out.takefirst();
		cc.busy = 1;
	}
}

put(cc: ref Conn, r: ref Req)
{
	cc.out.append(r);
	write(cc);
}

tkdisconnect(cc: ref Conn)
{
	tkcmd(".c.connect configure -text connect");
	tkcmd("destroy .p");
	tkcmd("panel .p");
	tkcmd("pack .p");
	tkcmd("pack forget .c.pass");
	if(connpid >= 0)
		kill(connpid);
	connpid = -1;
	if(cc != nil)
		for(l := cc.pids; l != nil; l = tl l)
			kill(hd l);
}

connect(pidc: chan of int)
{
	tkcmd(".c.connect configure -text disconnect");
	tkmsg("connecting...");
	tkcmd("update");
	pidc <-= pid();
	cc := ref Conn;
	cc.incr = 0;
	cc.ptrvnc = 0;

	cc.drawing = 0;
	cc.nfbc = chan of ref Image;
	cc.fbgetc = chan of chan of ref Image;
	cc.fbpxc = chan of (Rect, array of byte, int);
	cc.fbdrawc = chan of (Rect, ref Image, Point, int);
	cc.fbselfdrawc = chan of (Rect, Point, int);
	cc.fbfbdrawc = chan of (ref Image, Rect, Point);
	cc.fbflushc = chan of Rect;

	cc.outc = chan of ref Req;
	cc.busy = 0;
	cc.out = ref List[ref Req];

	err: string;
	{
		xconnect(cc);
		connc <-= (cc, nil);
		return;
	} exception ex {
	"x:*" =>
		err = ex[2:];
	"r:*" =>
		err = ex[2:];
		if(cc.read != nil)
			err = sprint("reading %s: %s", cc.read, err);
	"w:*" =>
		err = ex[2:];
		if(cc.write != nil)
			err = sprint("writing %s: %s", cc.write, err);
			err += ":"+cc.write;
	}
	connc <-= (cc, err);
}

xconnect(cc: ref Conn)
{
	fd: ref Sys->FD;
	if(lflag) {
		c := dial->announce(addr);
		if(c == nil)
			raise sprint("x:announce: %r");
		lc := dial->listen(c);
		if(lc == nil)
			raise sprint("x:listen: %r");
		fd = dial->accept(lc);
		if(fd == nil)
			raise sprint("x:accept: %r");
	} else {
		c := dial->dial(addr, nil);
		if(c == nil)
			raise sprint("x:dial: %r");
		fd = c.dfd;
	}

	say("dialed "+addr);
	i := bufio->fopen(fd, bufio->OREAD);
	o := bufio->fopen(fd, bufio->OWRITE);
	if(i == nil || o == nil)
		raise sprint("x:fopen: %r");
	cc.i = i;
	cc.o = o;

	cc.read = cc.write = "version handshake";
	vs := string bgn(i, 12);
	if(len vs != 12 || !str->prefix("RFB ", vs) || vs[len vs-1] != '\n')
		raise "x:remote does not speak RFB protocol";
	(maj, majr) := str->toint(vs[4:4+3], 10);
	(min, minr) := str->toint(vs[4+4:4+4+3], 10);
	if(majr != nil || minr != nil)
		raise sprint("x:bad version from remote (%q)", vs);
	if(maj != 3)
		raise sprint("x:unsupported major version %d", maj);
	lmin := min;
	if(lmin != 7 && lmin != 8)
		lmin = 3;
	bpn(o, sys->aprint("RFB %03d.%03d\n", maj, lmin));
	bflush(o);
	say(sprint("agreed on version (remote sent %d.%d, we decided on %d.%d)", maj, min, maj, lmin));

	cc.read = cc.write = "security handshake";
	sec: array of byte;
	if(lmin == 3) {
		t := bg32(i);
		case t {
		0 =>
			raise "x:security handshake: "+bgstr(i);
		1 or 2 =>
			{}
		* =>
			raise sprint("x:bad single security type from remote: %d", t);
		}
		sec = array[] of {byte t};
	} else {
		nsec := bg8(i);
		if(nsec == 0)
			raise "x:no security types available at server: "+bgstr(i);
		sec = bgn(i, nsec);  # security types
	}
	anone := avncauth := aother := 0;
	for(j := 0; j < len sec; j++)
		case int sec[j] {
		Anone =>	anone = 1;
		Avncauth =>	avncauth = 1;
		Abad =>		{}
		* =>		aother = 1;
		}
	if(anone) {
		say("using security type Anone");
		if(lmin != 3) {
			bp8(o, Anone);  # security type Anone
			bflush(o);
		}
	} else if(avncauth) {
		say("using security type Avncauth");
		if(lmin != 3) {
			bp8(o, Avncauth);  # security type Avncauth
			bflush(o);
		}
		chal := bgn(i, 16); # security challenge

		if(password == nil) {
			passc <-= pwc := chan of string;
			password = <-pwc;
		}
		dst := kr->dessetup(mkkey(password), nil);
		if(dst == nil)
			raise sprint("x:dessetup: %r");
		kr->desecb(dst, chal, len chal, kr->Encrypt);
		bpn(o, chal);  # response
		bflush(o);
	} else if(aother)
		raise "x:only unsupported security types...";
	else
		raise "x:no valid security types from server";

	if(!(anone && (lmin == 3 || lmin == 7))) {
		st := bg32(i);
		say(sprint("security handshake result %#x", st));
		if(st != 0) {
			password = nil;
			if(lmin == 3 || lmin == 7)
				raise "x:authentication fialed";
			raise "x:security handshake: "+bgstr(i);
		}
	}
	say("security handshake ok");

	xwclientinit(cc, !Sflag);
	si := xrserverinit(cc);
	say(sprint("serverinit, name %q, size %d.%d, pixelformat %s", si.name, si.width, si.height, si.pf.text()));
	cc.si = si;

	if(pixelformat != nil) {
		xwpixelformat(cc, pixelformat);
		cc.si.pf = *pixelformat;
	}
	xwencs(cc, encodings);
	xwfbupreq(cc, cc.incr, Rect((0,0), (cc.si.width,cc.si.height)));
	cc.incr = 1;
}

mkkey(s: string): array of byte
{
	pwd := array of byte s;
	if(len pwd < 8) {
		dd := array[8] of {* => byte 0};
		dd[:] = pwd;
		pwd = dd;
	} else
		pwd = pwd[:8];
	inv := array[16] of byte;
	for(k := 0; k < 16; k++) {
		(b0, b1, b2, b3) := (k>>0&1, k>>1&1, k>>2&1, k>>3&1);
		inv[k] = byte (b0<<3|b1<<2|b2<<1|b3<<0);
	}
	for(k = 0; k < 8; k++)
		pwd[k] = byte inv[int pwd[k]>>0 & 16rF]<<4 | byte inv[int pwd[k]>>4 & 16rF];
	return pwd;
}

enclookup(e: string): (int, int)
{
	for(i := 0; i < len encstrs; i++)
		if(encstrs[i].t1 == e)
			return (1, encstrs[i].t0);
	return (0, 0);
}

# restore image under ptr
ptrrestore(cc: ref Conn)
{
	if(cc == nil || cc.ptrbackup == nil || cc.drawing)
		return;

	br := cc.ptrbackup.r.addpt(cc.ptrbackuporig);
	cc.fbdrawc <-= (br, cc.ptrbackup, ZP, 1);
	cc.ptrbackup = nil;
}

# save image under ptr & write ptr
ptrdraw(cc: ref Conn)
{
	if(cc == nil || cc.ptrimg == nil || cc.drawing)
		return;

	cc.ptrbackup = disp.newimage(cc.ptrimg.r, draw->RGB24, 0, draw->Nofill);
	cc.ptrbackuporig = cc.ptr.sub(cc.ptrhotspot);
	cc.fbfbdrawc <-= (cc.ptrbackup, cc.ptrimg.r, cc.ptrbackuporig);
	cc.fbdrawc <-= (cc.ptrimg.r.addpt(cc.ptrbackuporig), cc.ptrimg, ZP, 1);
}

tkgetorigin(cc: ref Conn)
{
	x := int tkcmd(".p screenx 0");
	y := int tkcmd(".p screeny 0");
	cc.fborig = Point(x, y);
	say(sprint("new origin: %d.%d", x, y));
}

fb(cc: ref Conn, fb: ref Image, pidc: chan of int)
{
	pidc <-= pid();
	for(;;)
	alt {
	(r, d, fl) := <-cc.fbpxc =>
		fb.writepixels(r, d);
		if(fl)
			flushrect(fb, r);

	(r, i, p, fl) := <-cc.fbdrawc =>
		fb.draw(r, i, nil, p);
		if(fl)
			flushrect(fb, r);

	(r, p, fl) := <-cc.fbselfdrawc =>
		fb.draw(r, fb, nil, p);
		if(fl)
			flushrect(fb, r);

	<-cc.fbflushc =>
		flushrect(fb, fb.r);

	fb = <-cc.nfbc =>
		{}

	rc := <-cc.fbgetc =>
		fb.flush(draw->Flushnow);
		rc <-= fb;
		fb = <-cc.nfbc;

	(i, r, p) := <-cc.fbfbdrawc =>
		fb.flush(draw->Flushnow);
		i.draw(r, fb, nil, p);
	}
}

flushrect(fb: ref Image, r: Rect)
{
	fb.flush(draw->Flushnow);
	tkcmd(sprint(".p dirty %d %d %d %d; update", r.min.x, r.min.y, r.max.x, r.max.y));
}


vncread(cc: ref Conn, fbrqc: chan of int, pidc: chan of int)
{
	pidc <-= pid();
	err: string;
	{
		xvncread(cc, fbrqc);
		return;
	} exception ex {
	"x:*" =>
		err = ex[2:];
	"r:*" =>
		err = ex[2:];
		if(cc.read != nil)
			err = sprint("reading %s: %s", cc.read, err);
	"w:*" =>
		err = ex[2:];
		if(cc.write != nil)
			err = sprint("writing %s: %s", cc.write, err);
			err += ":"+cc.write;
	}
	connc <-= (nil, err);
}

xvncread(cc: ref Conn, fbrqc: chan of int)
{
	rc := chan of int;
	for(;;)
	case m := bg8(cc.i) {
	Sfbup =>
		unptrc <-= rc;
		<-rc;
		xrfbupdate(cc);
		fbrqc <-= 1;
	Scolmap =>
		xrcolormap(cc);
		raise "x:colormaps unsupported";
	Sbell =>
		# has no parameters
		s := "beep!";
		if(cc.beep)
			s = sprint("%10s", s);
		else
			s = sprint("%-10s", s);
		cc.beep = !cc.beep;
		tkcmd(".c.msg configure -text "+tk->quote(s));
		tkcmd("update");
	Stext =>
		t := xrservertext(cc);
		tkclient->snarfput(t);
		say("text from server: "+t);
	* =>
		raise sprint("x:unknown message from server: %d/%#x", m, m);
	}

}

vncwrite(cc: ref Conn, pidc: chan of int)
{
	pidc <-= pid();

	for(;;) {
		pick r := <-cc.outc {
		Fbup =>
			xwfbupreq(cc, r.incr, r.r);
		Ptr =>
			xwptr(cc, r.p);
		Snarf =>
			xwctext(cc, r.s);
		Key =>
			Xlctl: con 16rffe3;
			if(r.ctl)
				xwkey(cc, 1, Xlctl);
			xwkey(cc, 1, r.c);
			xwkey(cc, 0, r.c);
			if(r.ctl)
				xwkey(cc, 0, Xlctl);
		* =>
			raise "bad vncwrite";
		}
		wrotec <-= 1;
	} exception ex {
	"x:*" or
	"r:*" or
	"w:*" =>
		m := ex[2:];
		if(ex[0] == 'r' && cc != nil && cc.read != nil)
			m = sprint("reading %s: %s", cc.read, m);
		if(ex[0] == 'w' && cc != nil && cc.write != nil)
			m = sprint("writing %s: %s", cc.write, m);
		connc <-= (nil, m);
	}
}


xwclientinit(cc: ref Conn, shared: int)
{
	cc.write = "client init";
	b := cc.o;
	if(shared)
		shared = 1;
	bp8(b, shared);
	bflush(b);
}

xwfbupreq(cc: ref Conn, incr: int, r: Rect)
{
	cc.write = "fb update request";
	b := cc.o;
	bp8(b, Cfbup);
	bp8(b, incr);
	bp16(b, r.min.x);
	bp16(b, r.min.y);
	bp16(b, r.dx());
	bp16(b, r.dy());
	bflush(b);
}

xwptr(cc: ref Conn, p: Pointer)
{
	cc.write = "mouse move/buttons";
	b := cc.o;
	bp8(b, Cptr);
	bp8(b, p.buttons);
	bp16(b, p.xy.x);
	bp16(b, p.xy.y);
	bflush(b);
}

xwkey(cc: ref Conn, down, c: int)
{
	cc.write = "key press/release";
	b := cc.o;
	bp8(b, Ckey);
	bp8(b, down);
	bp16(b, 0); # pad
	bp32(b, c);
	bflush(b);
}

xwpixelformat(cc: ref Conn, pf: ref Pixelformat)
{
	cc.write = "pixel format";
	b := cc.o;
	bp8(b, Cpxfmt);
	bp24(b, 0); # pad
	bp8(b, pf.bitpp);
	bp8(b, pf.depth);
	bp8(b, pf.bigendian);
	bp8(b, pf.truecolor);
	bp16(b, pf.max.t0);
	bp16(b, pf.max.t1);
	bp16(b, pf.max.t2);
	bp8(b, pf.shift.t0);
	bp8(b, pf.shift.t1);
	bp8(b, pf.shift.t2);
	bp24(b, 0); # pad
	bflush(b);
}

xwencs(cc: ref Conn, e: list of int)
{
	cc.write = "supported encodings";
	b := cc.o;
	bp8(b, Cenc);
	bp8(b, 0); # pad
	bp16(b, len e);
	for(; e != nil; e = tl e)
		bp32(b, hd e);
	bflush(b);
}

xwctext(cc: ref Conn, s: string)
{
	cc.write = "snarf buffer";
	b := cc.o;
	(st, buf) := tolatin1->stob(convcs->Startstate, s);
	(nil, obuf) := tolatin1->stob(st, nil);
	if(len obuf != 0)
		raise "obuf not empty";
	bp8(b, Ctext);
	bp24(b, 0); # pad
	bp32(b, len buf);
	bpn(b, buf);
	bflush(b);
}


xrserverinit(cc: ref Conn): Serverinit
{
	cc.read = "server init";
	b := cc.i;
	width := bg16(b);
	height := bg16(b);
	pf := bgpixelformat(b);
	name := string bgn(b, bg32(b));
	return Serverinit(width, height, pf, name);
}

xrcolormap(cc: ref Conn)
{
	cc.read = "colormap";
	b := cc.i;
	bg8(b); # pad
	fc := bg16(b);
	nc := bg16(b);
	say(sprint("colormap, first color %d, %d colors", fc, nc));
	for(i := 0; i < nc; i++) {
		r := bg16(b);
		g := bg16(b);
		bb := bg16(b);
		say(sprint("color %d (%d): %d,%d,%d", i, i+fc, r, g, bb));
	}
}

xrservertext(cc: ref Conn): string
{
	cc.read = "servertext";
	b := cc.i;
	bgn(b, 3);
	dat := bgn(b, bg32(b));
	(nil, s, nil) := fromlatin1->btos(convcs->Startstate, dat, -1);
	return s;
}

xrfbupdate(cc: ref Conn)
{
	cc.read = "fb update";
	b := cc.i;
	bg8(b); # pad
	nr := bg16(b);
	if(dflag) say(sprint("framebuffer update, %d rects", nr));
	for(i := 0; i < nr; i++)
		xrect(cc);
}

bgx(b: ref Iobuf, by, be: int): int
{
	v := 0;
	if(be) {
		while(by-- > 0)
			v |= v<<8|bg8(b);
	} else {
		o := 0;
		for(i := 0; i < by; i++) {
			v |= bg8(b)<<o;
			o += 8;
		}
	}
	return v;
}

bgnpx(bb: ref Iobuf, n: int, cc: ref Conn, cpx3, alpha: int): array of byte
{
	by := cc.si.pf.bitpp/8;
	if(cpx3)
		by = 3;
	be := cc.si.pf.bigendian;
	imgby := 3;
	if(alpha)
		imgby = 4;
	d := array[n*imgby] of byte;
	o := 0;
	(mr, mg, mb) := cc.si.pf.max;
	(sr, sg, sb) := cc.si.pf.shift;
	(ur, ug, ub) := cc.si.pf.unshift;
	while(n-- > 0) {
		v := bgx(bb, by, be);
		# rgb24
		if(alpha)
			d[o++] = byte 16rff;
		d[o+0] = byte (v>>sb&mb)<<ub;
		d[o+1] = byte (v>>sg&mg)<<ug;
		d[o+2] = byte (v>>sr&mr)<<ur;
		o += 3;
	}
	return d;
}

bgpx(b: ref Iobuf, cc: ref Conn, cpx3: int): ref Image
{
	i := disp.newimage((ZP,(1,1)), draw->RGB24, 1, draw->Nofill);
	d := bgnpx(b, 1, cc, cpx3, 0);
	i.writepixels(i.r, d);
	return i;
}

xrect(cc: ref Conn)
{
	b := cc.i;
	x := bg16(b);
	y := bg16(b);
	w := bg16(b);
	h := bg16(b);
	e := bg32(b);
	r := Rect((x,y),(x+w,y+h));
	if(dflag) say(sprint("rect x %d, y %d, width %d, height %d, e %d/%#x", x, y, w, h, e, e));
	case e {
	Eraw =>
		d := bgnpx(b, w*h, cc, 0, 0);
		cc.fbpxc <-= (r, d, 1);
	Ecopyrect =>
		ox := bg16(b);
		oy := bg16(b);
		p := Point(ox, oy);
		cc.fbselfdrawc <-= (r, p, 1);
	Erre =>
		nr := bg32(b);
		bg := bgpx(b, cc, 0);
		cc.fbdrawc <-= (r, bg, ZP, 0);
		for(i := 0; i < nr; i++) {
			fg := bgpx(b, cc, 0);

			xx := bg16(b);
			yy := bg16(b);
			ww := bg16(b);
			hh := bg16(b);
			p0 := r.min.add((xx,yy));
			rr := Rect(p0, p0.add((ww,hh)));
			cc.fbdrawc <-= (rr, fg, ZP, 0);
		}
		cc.fbflushc <-= cc.fbr;
	Ehextile =>
		# tiles are max 16x16, left-right top-bottom
		xe := x+w;
		ye := y+h;
		hex := ref Hex;
		for(ys := y; ys < ye; ys += 16) {
			th := min(16, ye-ys);
			for(xs := x; xs < xe; xs += 16) {
				tw := min(16, xe-xs);
				tr := Rect((xs,ys), (xs+tw, ys+th));
				hextile(cc, tr, hex);
			}
		}
		cc.fbflushc <-= cc.fbr;
	#Etrle =>
	Ezrle =>
		# all messages are part of a single zlib pseudo-stream
                # inside are trle-like msgs, but with 64x64 pixel
                # tiles, and without subencodings 127 and 129.
		zbuf := bgn(b, bg32(b));
		if(cc.zz == nil)
			cc.zz = mkz();
		buf := infl(cc.zz, zbuf);
		say(sprint("zrle, zbuf %d, buf %d", len zbuf, len buf));
		zb := bufio->aopen(buf);

		xe := x+w;
		ye := y+h;
		for(ys := y; ys < ye; ys += 64) {
			th := min(64, ye-ys);
			for(xs := x; xs < xe; xs += 64) {
				tw := min(64, xe-xs);
				tr := Rect((xs,ys), (xs+tw, ys+th));
				tile(cc, zb, tr);
				cc.fbflushc <-= tr;
			}
		}

	Ecursor =>
		# new cursor from remote
		dat := bgnpx(b, w*h, cc, 0, 1);
		mw := (w+8-1)/8;
		msk := bgn(b, mw*h);
		o := 0;
		mo := 0;
		for(i := 0; i < h; i++) {
			for(j := 0; j < w; j++) {
				if((int msk[mo+j/8] & (1<<(8-1-j%8))) == 0) {
					dat[o+0] = byte 0;
					dat[o+1] = byte 0;
					dat[o+2] = byte 0;
					dat[o+3] = byte 0;
				}
				o += 4;
			}
			mo += mw;
		}
		cr := Rect(ZP,(w,h));
		img := disp.newimage(cr, draw->RGBA32, 0, draw->Black);
		img.writepixels(cr, dat);
		cursorc <-= (img, Point(x,y));

	Eresize =>
		# resize from remote
		resizec <-= (w, h);
	* =>
		raise sprint("x:unsupported encoding %d/%#x", e, e);
	}
}

Hex: adt {
	bg:	ref Image;
	fg:	ref Image;
};

hextile(cc: ref Conn, tr: Rect, hex: ref Hex)
{
	b := cc.i;
	f := bg8(b);
	Fraw, Fbg, Ffg, Fsubrects, Fsubcol: con 1<<iota;
	#say(sprint("hextile, f %#x, %d.%d sz %d.%d", f, tr.min.x, tr.min.y, tr.dx(), tr.dy()));
	if(f & Fraw) {
		d := bgnpx(b, tr.dx()*tr.dy(), cc, 0, 0);
		cc.fbpxc <-= (tr, d, 0);
		hex.fg = hex.bg = nil;
		return;
	}

	if(f & Fbg)
		hex.bg = bgpx(b, cc, 0);
	if(hex.bg == nil)
		raise "x:bad hextile, no bg";
	cc.fbdrawc <-= (tr, hex.bg, ZP, 0);
	if(f & Ffg)
		hex.fg = bgpx(b, cc, 0);
	if((f & Fsubrects) == 0)
		return;
	nr := bg8(b);
	if(f & Fsubcol)
		hex.fg = nil;
	else if(hex.fg == nil)
		raise "x:bad hextile, no fg";
	fg := hex.fg;
	for(i := 0; i < nr; i++) {
		if(f & Fsubcol)
			fg = bgpx(b, cc, 0);
		xy := bg8(b);
		wh := bg8(b);
		(x, y) := (xy>>4&16rf, xy>>0&16rf);
		(w, h) := ((wh>>4&16rf)+1, (wh>>0&16rf)+1);
		p0 := tr.min.add((x,y));
		cc.fbdrawc <-= ((p0,p0.add((w,h))), fg, ZP, 0);
	}
}

tile(cc: ref Conn, b: ref Iobuf, tr: Rect)
{
	cpx3 := cc.si.pf.cpx3;
	npx := tr.dx()*tr.dy();
	se := bg8(b);
	if(dflag) say(sprint("tile %d.%d - %d.%d, sz %d.%d, se %d", tr.min.x, tr.min.y, tr.max.x, tr.max.y, tr.dx(), tr.dy(), se));
	case se {
	0 =>
		# raw
		if(dflag) say("raw");
		d := bgnpx(b, npx, cc, cpx3, 0);
		cc.fbpxc <-= (tr, d, 0);
	1 =>
		# solid
		if(dflag) say("solid");
		d := bgpx(b, cc, cpx3);
		cc.fbdrawc <-= (tr, d, ZP, 0);
	2 to 16 =>
		# packed palette
		if(dflag) say("packed palette");
		npl := se;
		pal := array[npl] of array of byte;
		for(j := 0; j < npl; j++)
			pal[j] = bgnpx(b, 1, cc, cpx3, 0);

		# packed data.  high bits of v are always next to use, with nb of them available.
		bits: int;
		case npl {
		2 =>		bits = 1;
		3 or 4 =>	bits = 2;
		* =>		bits = 4;
		}
		say(sprint("packed palette, ncol %d, %d bits", npl, bits));
		shift := 8-bits;
		mask := (1<<bits)-1;
		nb := 0;
		v := 0;

		d := array[3*npx] of byte;
		o := 0;
		xx := 0;
		while(o < len d) {
			if(nb == 0 || xx == 0) {
				v = bg8(b);
				nb = 8;
			}
			i := (v>>shift) & mask;
			if(i >= len pal)
				raise "bad packed color index";
			d[o:] = pal[i];
			o += 3;
			v <<= bits;
			nb -= bits;
			xx = (xx+1)%tr.dx();
		}
		cc.fbpxc <-= (tr, d, 0);
	17 to 126 =>
		raise sprint("x:invalid subencoding %d", se);
	127 =>
		# reused packet palette
		raise "x:bad packet palette subencoding in zrle";
	128 =>
		# plain rle
		say("plain rle");
		d := array[3*npx] of byte;
		o := 0;
		while(o < len d) {
			px := bgnpx(b, 1, cc, cpx3, 0);
			n := getrl(b);
			if(o+3*n > len d)
				raise "plain rle data too long";
			while(n-- > 0) {
				d[o:] = px;
				o += 3;
			}
		}
		cc.fbpxc <-= (tr, d, 0);
	129 =>
		# reused palette rle
		raise "x:bad palette rle subencoding in zrle";
	130 to 255 =>
		# palette rle
		say("palette rle");
		npl := se-128;
		pal := array[npl] of array of byte;
		for(j := 0; j < npl; j++)
			pal[j] = bgnpx(b, 1, cc, cpx3, 0);
		d := array[3*npx] of byte;
		o := 0;
		while(o < len d) {
			c := bg8(b);
			n := 1;
			if(c & 16r80) {
				n = getrl(b);
				c &= 16r7f;
			}
			if(o+3*n > len d)
				raise "palette rle data too long";
			if(c >= len pal)
				raise "bad palette rle color index";
			while(n-- > 0) {
				d[o:] = pal[c];
				o += 3;
			}
		}
		cc.fbpxc <-= (tr, d, 0);
	}
}

getrl(b: ref Iobuf): int
{
	n := 1;
	for(;;) {
		nn := bg8(b);
		n += nn;
		if(nn != 255)
			return n;
	}
}

mkz(): ref Z
{
	z := ref Z;
	z.rqc = inflate->start("z");
	pick r := <-z.rqc {
	Start =>
		z.pid = r.pid;
	* =>
		raise "bad filter";
	}
	return z;
}

infl(z: ref Z, d: array of byte): array of byte
{
	o := 0;
	if(z.f != nil) {
		say("inflate, using old fill");
		o = min(len d, len z.f.buf);
		z.f.buf[:] = d[:o];
		z.f.reply <-= o;
		z.f = nil;
	}

	buf := array[0] of byte;
	for(;;)
	pick r := <-z.rqc {
	Start =>
		raise "bad filter";
	Fill =>
		say(sprint("fill, d %d, o %d", len d, o));
		if(len r.buf == 0)
			raise "bad filter";
		if(o == len d) {
			z.f = r;
			return buf;
		}
		n := min(len d-o, len r.buf);
		r.buf[:] = d[o:o+n];
		r.reply <-= n;
		o += n;
	Result =>
		say(sprint("inflate, result, %d", len r.buf));
		nb := array[len buf+len r.buf] of byte;
		nb[:] = buf;
		nb[len buf:] = r.buf;
		buf = nb;
		r.reply <-= 0;
	Finished =>
		raise "inflate finished";
	Info =>
		say("inflate info: "+r.msg);
	Error =>
		raise "inflate: "+r.e;
	}
}


bg8(b: ref Iobuf): int
{
	c := b.getb();
	if(c == bufio->EOF)
		raise "r:eof";
	if(c == bufio->ERROR)
		raise sprint("r:read: %r");
	return c;
}

bg16(b: ref Iobuf): int
{
	b1 := bg8(b);
	b0 := bg8(b);
	return b1<<8 | b0<<0;
}

bg32(b: ref Iobuf): int
{
	b32 := bg16(b);
	b10 := bg16(b);
	return b32<<16 | b10<<0;
}

bgpixelformat(b: ref Iobuf): Pixelformat
{
	pf: Pixelformat;
	pf.bitpp	= bg8(b);
	pf.depth	= bg8(b);
	pf.bigendian	= bg8(b);
	pf.truecolor	= bg8(b);
	pf.max.t0	= bg16(b);
	pf.max.t1	= bg16(b);
	pf.max.t2	= bg16(b);
	pf.shift.t0	= bg8(b);
	pf.shift.t1	= bg8(b);
	pf.shift.t2	= bg8(b);
	bgn(b, 3);

	case pf.bitpp {
	8 or 16 or 32 =>
		{}
	* =>
		raise sprint("x:invalid bits per pixel: %d", pf.bitpp);
	}
	pf.unshift = (8-log2(pf.max.t0+1), 8-log2(pf.max.t1+1), 8-log2(pf.max.t2+1));
	pf.cpx3 = pf.bitpp==32 && pf.depth<=24;
	return pf;
}

log2(v: int): int
{
	for(i := -1; v > 0; i++)
		v >>= 1;
	return i;
}

bgn(b: ref Iobuf, n: int): array of byte
{
	d := array[n] of byte;
	h := 0;
	while(h < len d) {
		nn := b.read(d[h:], len d-h);
		if(nn == 0 || nn == bufio->EOF)
			raise "r:short read";
		if(nn == bufio->ERROR)
			raise sprint("r:read: %r");
		h += nn;
	}
	return d;
}

bgstr(b: ref Iobuf): string
{
	return string bgn(b, bg32(b));
}


bflush(b: ref Iobuf)
{
	if(b.flush() == bufio->ERROR)
		raise sprint("w:flush: %r");
}

bp8(b: ref Iobuf, v: int)
{
	if(b.putb(byte v) == bufio->ERROR)
		raise sprint("w:write: %r");
}

bp16(b: ref Iobuf, v: int)
{
	bp8(b, v>>8);
	bp8(b, v>>0);
}

bp24(b: ref Iobuf, v: int)
{
	bp8(b, v>>16);
	bp8(b, v>>8);
	bp8(b, v>>0);
}

bp32(b: ref Iobuf, v: int)
{
	bp16(b, v>>16);
	bp16(b, v>>0);
}

bpn(b: ref Iobuf, d: array of byte)
{
	if(b.write(d, len d) != len d)
		raise sprint("w:write: %r");
}


tkmsg(m: string)
{
	tkcmd(".c.msg configure -text "+tk->quote(m));
}

tkcmd(s: string): string
{
	r := tk->cmd(top, s);
	if(r != nil && r[0] == '!')
		warn(sprint("tkcmd: %q: %s", s, r));
	return r;
}

progctl(pid: int, s: string)
{
	sys->fprint(sys->open(sprint("/prog/%d/ctl", pid), Sys->OWRITE), "%s", s);
}

kill(pid: int)
{
	progctl(pid, "kill");
}

pid(): int
{
	return sys->pctl(0, nil);
}

revint(l: list of int): list of int
{
	r: list of int;
	for(; l != nil; l = tl l)
		r = hd l::r;
	return r;
}

min(a, b: int): int
{
	if(a < b)
		return a;
	return b;
}

say(s: string)
{
	if(dflag)
		warn(s);
}

fd2: ref Sys->FD;
warn(s: string)
{
	if(fd2 == nil)
		fd2 = sys->fildes(2);
	sys->fprint(fd2, "%s\n", s);
}

fail(s: string)
{
	warn(s);
	raise "fail:"+s;
}
