include Makefile.howtofort
#
# Makefile.f2c or .f77 will have
# - all: orbit rule
# - orbitspice.o: orbitspice.f rule
# - CC, GCC, LFC, CFLAGS, GCFLAGS, FFLAGS, LIBS variables
#
include Makefile.$(HOWTOFORT)

OBJS1LESSTIFBUGWITHLTBUG = orbitgui_cas.o orbitgui_util.o
OBJS1LESSTIFBUGWOLTBUG = orbitgui_cas_woltbug.o orbitgui_util_woltbug.o

OBJS1base = spud.o orbitgui_enterxyz.o orbitgui_enterpoint.o \
       orbitgui_elts.o \
       orbitgui_selnis.o \
       orbitgui_gen.o orbitgui_fieldmenu.o \
       orbitgui_shading.o \
       orbitgui_gen_msi0.o \
       orbitgui_entergentimes.o orbit_gentimes.o \
       orbit_gen_msirep.o orbit_gen_nisrep.o \
       orbitgui_gen_nis0.o \
       orbit_frames.o orbit_msi_frames.o orbit_nis_frames.o \
       orbit_flgife.o orbit_flgifc.o \
       orbitgui_framesout.o \
       orbitgui_ps.o orbitgui_xfig.o \
       orbit_stats.o \
       orbit_cas_sasf.o orbit_cas_sasf2CAS.o orbit_cas_opnav.o \
       orbitspice.o orbit3d.o orbit_cas.o orbit_util.o \
       $(SPUDOBJS)

       
OBJS1 = $(OBJS1base) $(OBJS1LESSTIFBUGWITHLTBUG) \
        orbitgui.o orbitfort.o orbit_fov.o
       
OBJS1WOLTBUG = $(OBJS1base) $(OBJS1LESSTIFBUGWOLTBUG) \
        orbitgui.o orbitfort.o orbit_fov.o

OBJS2_SUBS= toiep.o spudprint.o orbit_stats.o orbdb.o $(OBJS0)
OBJS2_DEBUG_SUBS= spudprint.o orbit_stats.o orbdb.debug.o $(OBJS0)
OBJS2= frm2iep.o $(OBJS2_SUBS)
OBJSkmetquat2ck= kmetquat2ck.o orbitspice.o

OBJS0= orbitspice.o orbit3d.o orbit_util.o orbitfort.o orbit_fov.o $(SPUDOBJS)

SPUDOBJS= spudface.o spudmisc.o spudshap.o spudview.o \
          spudplates.o

SPUDOBJS2= $(SPUDOBJS) spudview2.o

CCF=$(CC) $(CFLAGS)
CCFNOX=$(CC) $(CFLAGSNOX)

SPUD2EXES=spud2plate spud2xplate spud2oogl spud2raysh spudellfix2plate \
          spud2wrl spud2stl spud2topo platelandmarks
FRM2EXES=frm2iep frm2met

ORBIT3DDUMMY=orbit3d.o orbit3d_dummy_funcs.o
#TWOEXES=orbit2 speedtest2
TWOEXES=testspudf2

ALLEXES= \
  orbit matriculator $(FRM2EXES) genspud spudlims platetest speedtest spudtest \
  orbit.woltbug platecompare \
  cktimes $(SPUD2EXES) testatt simplespice time simple2 simple3 orbitsetter \
  getckav frame \
  kmetquat2ck fixupmsi msinis2quatmet msiq_exe superscan \
  orbit_wrl u2e frm2iep frm2iepgui \
  orbdbck orbdbMrg orbdbDump testorbdb \
  plt2metHtml orbdb2pltmet \
  $(TWOEXES)

allexes: $(ALLEXES) frm2iep.hlp

orbit: $(OBJS1) orbit_wrl
	$(LD) -o orbit $(OBJS1) $(LIBS)

orbit.woltbug: $(OBJS1WOLTBUG)
	$(LD) -o orbit.woltbug $(OBJS1WOLTBUG) $(LIBS)

screensavestuff: screensavestuff.o
	$(CC) -o screensavestuff screensavestuff.o \
		-L/usr/lesstif/lib -lXm \
		-L/usr/X11R6/lib -lXt -lX11

orbitgui_util_woltbug.o: orbitgui_util.o
	$(CCF) -D_LESSTIFBUG_H_ orbitgui_util.c -c -o $@

orbitgui_cas_woltbug.o: orbitgui_cas.o
	$(CCF) -D_LESSTIFBUG_H_ orbitgui_cas.c -c -o $@

orbitgui2.o: orbitgui.o
	$(CCF) -Dspudview=spudview2_ orbitgui.c -c -o orbitgui2.o

orbitfort2.o: orbitfort.o
	$(CCF) -Dnewspudv=newspudv2_ orbitfort.c -c -o orbitfort2.o

kmetquat2ck: $(OBJSkmetquat2ck)
	$(LD) -o kmetquat2ck $(OBJSkmetquat2ck) $(FLIBS)
	rm -f metquat2ck sclkquat2ck
	ln -s kmetquat2ck metquat2ck
	ln -s kmetquat2ck sclkquat2ck

frm2: $(FRM2EXES)

fixupmsi: fixupmsi.c
	$(CCF) -o fixupmsi fixupmsi.c

msinis2quatmet: msinis2quatmet.c
	$(CCF) -o msinis2quatmet msinis2quatmet.c orbit_util.o -lm

msiq_exe: msiq_exe.c
	$(CCF) -o msiq_exe msiq_exe.c orbit_util.o -lm

superscan: superscan.o
	$(CCF) -o superscan superscan.o orbit_util.o orbitspice.o $(FLIBS)

iofhapkelcrv.o: iofhapkelcrv.f iepcmn.inc iepcmn.par
	$(FC) $(FFLAGS) -c -o $@ iofhapkelcrv.f

orbitlcrv: orbitlcrv.o iofhapkelcrv.o
	$(CCF) -o $@ orbitlcrv.o iofhapkelcrv.o $(FLIBS)

plt2metHtml: plt2metHtml.o orbdb.o $(OBJS0)
	$(LD) -o plt2metHtml plt2metHtml.o orbdb.o $(OBJS0) $(DBLIBS) $(FLIBS)

orbdb2pltmet: orbdb2pltmet.o orbdb.o
	$(LD) -o $@ orbdb2pltmet.o orbdb.o $(DBLIBS) -lm

frm2iep.debug.o: frm2iep.c
	$(CCF) -g -O0 -c -o $@ frm2iep.c

orbdb.debug.o: orbdb.c
	$(CCF) -g -O0 -c -o $@ orbdb.c

frm2iep.debug: frm2iep.debug.o $(OBJS2_DEBUG_SUBS)
	$(LD) -o $@ -g -O0 frm2iep.debug.o $(OBJS2_DEBUG_SUBS) \
		$(DBLIBS_DEBUG) $(FLIBS)

u2e: u2e.o orbitspice.o
	$(LD) -o $@ u2e.o orbitspice.o $(FLIBS)

frm2iep: $(OBJS2)
	$(LD) -o frm2iep $(OBJS2) $(DBLIBS) $(FLIBS)

frm2iep_sub.o: frm2iep.c
	$(CCF) -D_NO_MAIN_ -c -o frm2iep_sub.o frm2iep.c

frm2iepgui: frm2iepgui.c \
		orbitgui_fieldmenu.o orbitgui_util.o \
		frm2iep_sub.o $(OBJS2_SUBS)
	$(CCF) -o frm2iepgui \
		frm2iepgui.c \
		orbitgui_fieldmenu.o orbitgui_util.o \
		frm2iep_sub.o $(OBJS2_SUBS) \
		$(DBLIBS) $(XLIBS) $(FLIBS)

fixOrbdb: fixOrbdb.o $(OBJS2_SUBS)
	$(LD) -o fixOrbdb fixOrbdb.o $(OBJS2_SUBS) $(DBLIBS) $(FLIBS)

frm2iep.hlp: frm2iep.c
	sed -e '/CUT FROM HERE FOR FRM2IEP.HLP/,/LAST LINE OF FRM2IEP.C/d' \
		-e 's,^[ /]\*[/]*,,' \
		< frm2iep.c > frm2iep.hlp

testatt_lpp: testatt_lpp.f
	$(LFC) $(FFLAGS) -o testatt_lpp testatt_lpp.f $(FLIBS)

testatt_lpp.out: testatt_lpp testatt_lpp.inp
	$(RM) testatt_lpp.out*
	testatt_lpp < testatt_lpp.inp > testatt_lpp.out.all
	grep '#1' testatt_lpp.out.all | sed 's/^ *#1//' > testatt_lpp.out.1on2
	grep '#2' testatt_lpp.out.all | sed 's/^ *#2//' > testatt_lpp.out.rates
	grep '#3' testatt_lpp.out.all | sed 's/^ *#3//' > testatt_lpp.out.tpc
	grep -v '#' testatt_lpp.out.all > testatt_lpp.out
	$(RM) testatt_lpp.out.all

frm2met: frm2met.f
	$(LFC) $(FFLAGS) -o frm2met frm2met.f $(FFLIBS)

frame: frame.f
	$(LFC) $(FFLAGS) -o frame frame.f $(FFLIBS)

genspud: genspud.o
	$(CCFNOX) -o genspud genspud.o -lm

veculator: veculator.o $(ORBIT3DDUMMY)
	$(CCFNOX) -o veculator veculator.o $(ORBIT3DDUMMY) $(FLIBS)

orbit_fov_test: orbit_fov.c $(ORBIT3DDUMMY)
	$(CCF) -D_ORBIT_FOV_TEST_ -o orbit_fov_test orbit_fov.c \
		$(ORBIT3DDUMMY) $(FLIBS)

matriculator: matriculator.o $(ORBIT3DDUMMY)
	$(CCFNOX) -o matriculator matriculator.o $(ORBIT3DDUMMY) $(FLIBS)

spudlims: spudlims.o $(SPUDOBJS)
	$(CCFNOX) -o spudlims spudlims.o $(SPUDOBJS) $(FLIBS)

testspudf2: testspudf2.o spudf2.o $(SPUDOBJS) $(ORBIT3DDUMY)
	$(CCF) -o testspudf2 testspudf2.o spudf2.o \
		$(SPUDOBJS) $(ORBIT3DDUMMY) $(FLIBS)

platecompare: platecompare.o $(SPUDOBJS) $(ORBIT3DDUMMY)
	$(CCF) -o platecompare platecompare.o $(SPUDOBJS) $(ORBIT3DDUMMY) -lm

platetest: platetest.o $(SPUDOBJS) $(ORBIT3DDUMMY)
	$(CCF) -o platetest platetest.o $(SPUDOBJS) $(ORBIT3DDUMMY) -lm

speedtest: speedtest.o $(SPUDOBJS) $(ORBIT3DDUMMY)
	$(CCF) -o speedtest speedtest.o $(SPUDOBJS) $(ORBIT3DDUMMY) -lm

speedtest2: speedtest2.o $(SPUDOBJS2) $(ORBIT3DDUMMY)
	$(CCF) -o speedtest2 speedtest2.o $(SPUDOBJS2) $(ORBIT3DDUMMY) -lm

spudtest: spudtest.o $(SPUDOBJS)
	$(CCFNOX) -o spudtest spudtest.o $(SPUDOBJS) $(FLIBS)

#met: met.o
#	$(LFC) $(FFLAGS) -o met met.o $(FFLIBS)

cktimes: cktimes.o
	$(LFC) $(FFLAGS) -o cktimes cktimes.o $(FFLIBS)

#cktest: cktest.o
#	$(LFC) $(FFLAGS) -o cktest cktest.o $(FFLIBS)

spud2: $(SPUD2EXES)

platelandmarks: platelandmarks.o spudshap.o spudface.o spudprint.o \
		spudplates.o spudview.o spudmisc.o
	$(CCFNOX) -o platelandmarks platelandmarks.o \
		spudshap.o spudface.o spudprint.o\
		spudplates.o spudview.o spudmisc.o \
		$(FLIBS)


spud2wrl: spud2wrl.o $(SPUDOBJS)
	$(CCFNOX) -o spud2wrl spud2wrl.o $(SPUDOBJS) $(FLIBS)

spud2stl: spud2stl.o $(SPUDOBJS) spudprint.o
	$(CCFNOX) -o spud2stl spud2stl.o $(SPUDOBJS) spudprint.o \
		$(FLIBS)

pltxlate: pltxlate.o spudshap.o spudface.o \
		spudplates.o spudview.o spudmisc.o
	$(CCFNOX) -o $@ pltxlate.o spudshap.o spudface.o \
		spudplates.o spudview.o spudmisc.o \
		$(FLIBS)

ospudxlate: ospudxlate.o spudshap.o spudface.o \
		spudplates.o spudview.o spudmisc.o
	$(CCFNOX) -o $@ ospudxlate.o spudshap.o spudface.o \
		spudplates.o spudview.o spudmisc.o \
		$(FLIBS)

pltintersect: pltintersect.o spudshap.o spudface.o \
		spudplates.o spudview.o spudmisc.o \
		$(ORBIT3DDUMMY)
	$(CCFNOX) -o $@ pltintersect.o spudshap.o spudface.o \
		spudplates.o spudview.o spudmisc.o \
		$(ORBIT3DDUMMY) $(FLIBS)

spud2plate: spud2plate.o spudshap.o spudface.o spudprint.o \
		spudplates.o spudview.o spudmisc.o
	$(CCFNOX) -o spud2plate spud2plate.o spudshap.o spudface.o spudprint.o \
		spudplates.o spudview.o spudmisc.o \
		$(FLIBS)

spud2topo: spud2topo.o spudshap.o spudface.o spudprint.o \
		spudplates.o spudview.o spudmisc.o
	$(CCFNOX) -o spud2topo spud2topo.o spudshap.o spudface.o spudprint.o \
		spudplates.o spudview.o spudmisc.o \
		$(FLIBS)

spudellfix2plate: spudellfix2plate.o \
		spudshap.o spudface.o spudprint.o \
		spudplates.o spudview.o spudmisc.o
	$(CCFNOX) -o spudellfix2plate spudellfix2plate.o spudshap.o spudface.o \
		spudprint.o \
		spudplates.o spudview.o spudmisc.o \
		$(FLIBS)

spud2xplate: spud2xplate.o $(SPUDOBJS) spudprint.o
	$(CCFNOX) -o spud2xplate spud2xplate.o $(SPUDOBJS) spudprint.o \
		$(FLIBS)

spud2oogl: spud2oogl.o spudshap.o spudface.o spudprint.o \
		spudplates.o spudview.o spudmisc.o
	$(CCFNOX) -o spud2oogl spud2oogl.o spudshap.o spudface.o spudprint.o \
		spudplates.o spudview.o spudmisc.o \
		$(FLIBS)

spud2raysh: spud2raysh.o spudshap.o spudface.o spudprint.o \
		spudplates.o spudview.o spudmisc.o
	$(CCFNOX) -o spud2raysh spud2raysh.o spudshap.o spudface.o spudprint.o \
		spudplates.o spudview.o spudmisc.o \
		$(FLIBS)

s2umesh: s2umesh.o spudshap.o spudface.o
	$(CCFNOX) -o s2umesh s2umesh.o spudshap.o spudface.o -lm

s2stoff: s2stoff.o spudshap.o spudface.o
	$(CCFNOX) -o s2stoff s2stoff.o spudshap.o spudface.o -lm

testatt: testatt.o
	$(LFC) $(FFLAGS) -o testatt testatt.o $(SPICELIBS) $(FFLIBS)

simplespice: simplespice.o
	$(LFC) $(FFLAGS) -o simplespice simplespice.o $(SPICELIBS) $(FFLIBS)

time: time.o
	$(LFC) $(FFLAGS) -o time time.o $(SPICELIBS) $(FFLIBS)

kmet2cal: kmet2cal.o
	$(LFC) $(FFLAGS) -o kmet2cal kmet2cal.o $(SPICELIBS) $(FFLIBS)

simple2: simple2.o
	$(LFC) $(FFLAGS) -o simple2 simple2.o $(SPICELIBS) $(FFLIBS)

getckav: getckav.o
	$(LFC) $(FFLAGS) -o getckav getckav.o $(SPICELIBS) $(FFLIBS)

simple3: simple3.o
	$(LFC) $(FFLAGS) -o simple3 simple3.o $(SPICELIBS) $(FFLIBS)

oldspacit: oldspacit.o
	$(LFC) $(FFLAGS) -o oldspacit oldspacit.o $(SPICELIBS) $(FFLIBS)

orbitsetter: orbitsetter.o $(ORBIT3DDUMMY)
	-$(LD) -o orbitsetter orbitsetter.o $(ORBIT3DDUMMY) $(LIBS)

orbit_wrl: orbit_wrl.source
	sed 's,_CPPCMD_,$(CPP),' < orbit_wrl.source > $@
	chmod a+x orbit_wrl

orbdbck: orbdbck.c orbdb.o
	$(CCF) -o $@ orbdbck.c orbdb.o $(DBLIBS) -lm

orbdbMrg: orbdbMrg.c orbdb.o
	$(CCF) -o $@ orbdbMrg.c orbdb.o $(DBLIBS) -lm

orbdbDump: orbdbDump.c orbdb.o
	$(CCF) -o $@ orbdbDump.c orbdb.o $(DBLIBS) -lm

testorbdb.o: orbdb.c orbdb.h
	$(CCF) -c -D_ORBDB_SIMPLETEST_ -o $@ orbdb.c

testorbdb: testorbdb.o
	$(CC) $(LDFLAGS) -o $@ testorbdb.o $(DBLIBS) -lm

%.wrl: %.plates_ifs orbit_default.wrl_cpp
	orbit_wrl -shape $< orbit_default.wrl_cpp $@

%.wrl: %.plate_ifs orbit_default.wrl_cpp
	orbit_wrl -shape $< orbit_default.wrl_cpp $@

%.plates_ifs: %.plates
	spud2wrl $< > $@

%.plate_ifs: %.plate
	spud2wrl $< > $@

%.i: %.c
	$(CPP) $(CFLAGS) $(^) > $(^:.c=.i)

clean:
	$(RM) *.o $(ALLEXES)

depend:
	cat /dev/null > Makefile.dependtmp
	makedepend $(CGENERAL) -fMakefile.dependtmp -w900 *.c
	sed 's, X[^ ]*,,g' Makefile.dependtmp | \
		sed 's, /usr[^ ]*,,g' | grep -v ': *$$' > Makefile.depend
	$(RM) Makefile.dependtmp

debug:
	make CDEBUG='-g -O0'

include Makefile.depend
