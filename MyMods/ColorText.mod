



<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<!-- ViewVC :: http://www.viewvc.org/ -->
<head>
<title>[gm2] Log of /gm2/examples/ncurses/ColorText.mod</title>
<meta name="generator" content="ViewVC 1.0.7" />
<link rel="stylesheet" href="/viewcvs-doc/styles.css" type="text/css" />

</head>
<body>
<div style="float: right; padding: 5px;"><a href="http://savannah.gnu.org/"><img src="http://savannah.gnu.org/images/common/floating.png" alt="Provided by GNU Savannah" width="148" height="125" /></a></div>
<h3>Savannah CVS Surfing


- project <a href="http://savannah.nongnu.org/projects/gm2">gm2</a></div>


- Log of /gm2/examples/ncurses/ColorText.mod
</h3>
<table style="margin:0; border-spacing:0" class="vc_navheader">
<tr>
<td>
<strong>

<a href="/viewvc/?root=gm2">

[gm2]</a>
/

<a href="/viewvc/gm2/?root=gm2">

gm2</a>
/

<a href="/viewvc/gm2/examples/?root=gm2">

examples</a>
/

<a href="/viewvc/gm2/examples/ncurses/?root=gm2">

ncurses</a>
/



ColorText.mod


</strong>

</td>
</tr>
</table>
<!-- It would be nice to detect a "web.cvs.s*.*gnu.org vhost, and use /web instead of /sources... -->




<code>cvs -d:pserver:anonymous@cvs.sv.gnu.org:/sources/gm2 co gm2/examples/ncurses/ColorText.mod</code>

<br />
<span style="font-size: smaller">(to check out web pages for a project, replace /sources with /web in the path above)</span>





<p style="margin:0;">

<a href="/viewvc/gm2/examples/ncurses/?root=gm2"><img src="/viewcvs-doc/images/back_small.png" width="16" height="16" alt="Parent Directory" /> Parent Directory</a>




</p>

<hr />
<table class="auto">



<tr>
<td>Links to HEAD:</td>
<td>
(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;view=markup">view</a>)
(<a href="/viewvc/*checkout*/gm2/examples/ncurses/ColorText.mod?root=gm2">download</a>)

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?view=annotate&amp;root=gm2">annotate</a>)
</td>
</tr>



<tr>
<td>Sticky Tag:</td>
<td><form method="get" action="/viewvc/gm2/examples/ncurses/ColorText.mod" style="display: inline">
<div style="display: inline">
<input type="hidden" name="view" value="log" /><input type="hidden" name="root" value="gm2" />


<select name="pathrev" onchange="submit()">
<option value=""></option>

<optgroup label="Branches">


<option>MAIN</option>


</optgroup>

<optgroup label="Non-branch tags">


<option>gm2_0_52</option>



<option>gm2_0_51</option>



<option>gm2_0_50</option>



<option>gm2_0_49</option>



<option>gm2_0_44</option>



<option>HEAD</option>


</optgroup>

</select>

<input type="submit" value="Set" />
</div>
</form>

</td>
</tr>
</table>
 








<div>
<hr />

<a name="rev1.11"></a>
<a name="HEAD"></a>


Revision <strong>1.11</strong> -

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?revision=1.11&amp;root=gm2&amp;view=markup">view</a>)

(<a href="/viewvc/*checkout*/gm2/examples/ncurses/ColorText.mod?revision=1.11&amp;root=gm2">download</a>)

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?annotate=1.11&amp;root=gm2">annotate</a>)



- <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.11&amp;view=log">[select for diffs]</a>




<br />

<em>Sun Oct  3 19:01:04 2010 UTC</em> (4 months, 3 weeks ago) by <em>gaius</em>


<br />Branch:

<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?view=log&amp;root=gm2&amp;pathrev=MAIN"><strong>MAIN</strong></a>



<br />CVS Tags:

<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?view=log&amp;root=gm2&amp;pathrev=HEAD"><strong>HEAD</strong></a>






<br />Changes since <strong>1.10: +2 -1 lines</strong>







<br />Diff to <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.10&amp;r2=1.11">previous 1.10</a>










<pre class="vc_log">* gm2/Make-lang.in:  tidied up Copyright dates on
* gm2/Makefile.in:  the following files.
* gm2/config-lang.in:
* gm2/gccgm2.c:
* gm2/gm2-common.c:
* gm2/gm2-common.h:
* gm2/gm2-lang.c:
* gm2/gm2-lang.h:
* gm2/gm2-tree.def:
* gm2/gm2-tree.h:
* gm2/gm2builtins.c:
* gm2/gm2config.h.in:
* gm2/gm2except.c:
* gm2/gm2spec.c:
* gm2/gm2version.h:
* gm2/lang-options.h:
* gm2/lang-specs.h:
* gm2/m2pp.c:
* gm2/m2pp.h:
* gm2/examples/callingC/Makefile.in:
* gm2/examples/callingC/hello.mod:
* gm2/examples/callingC/libprintf.def:
* gm2/examples/cplusplus/cppcatchm2/Makefile.in:
* gm2/examples/cplusplus/cppcatchm2/cpp.def:
* gm2/examples/cplusplus/cppcatchm2/m2.def:
* gm2/examples/cplusplus/cppcatchm2/m2.mod:
* gm2/examples/cplusplus/m2catchcpp/Makefile.in:
* gm2/examples/cplusplus/m2catchcpp/cpp.def:
* gm2/examples/cplusplus/m2catchcpp/m2.mod:
* gm2/examples/cpp/Makefile.in:
* gm2/examples/cpp/hello.mod:
* gm2/examples/cppDef/a.def:
* gm2/examples/cppDef/b.mod:
* gm2/examples/executive/testexecutive.mod:
* gm2/examples/executive/testtime.mod:
* gm2/examples/executive/winexec.mod:
* gm2/examples/gravity/Makefile.in:
* gm2/examples/gravity/deviceGnuPic.def:
* gm2/examples/gravity/deviceGnuPic.mod:
* gm2/examples/gravity/gsl.def:
* gm2/examples/gravity/roots.def:
* gm2/examples/gravity/roots.mod:
* gm2/examples/gravity/test.mod:
* gm2/examples/gravity/testsim.py:
* gm2/examples/gravity/twoDsim.def:
* gm2/examples/gravity/twoDsim.mod:
* gm2/examples/hello/Makefile.in:
* gm2/examples/hello/hello.mod:
* gm2/examples/iso/files/rot13.mod:
* gm2/examples/iso/hello/hello.mod:
* gm2/examples/iso/socket/client.mod:
* gm2/examples/map/AdvMap.def:
* gm2/examples/map/AdvMap.mod:
* gm2/examples/map/BoxMap.def:
* gm2/examples/map/BoxMap.mod:
* gm2/examples/map/Chance.def:
* gm2/examples/map/Chance.mod:
* gm2/examples/map/Find.def:
* gm2/examples/map/Find.mod:
* gm2/examples/map/Geometry.def:
* gm2/examples/map/Geometry.mod:
* gm2/examples/map/MakeBoxes.def:
* gm2/examples/map/MakeBoxes.mod:
* gm2/examples/map/Makefile.in:
* gm2/examples/map/Map.mod:
* gm2/examples/map/RoomMap.def:
* gm2/examples/map/RoomMap.mod:
* gm2/examples/map/Semantic.mod:
* gm2/examples/map/StoreCoord.def:
* gm2/examples/map/StoreCoord.mod:
* gm2/examples/map/StoreCoords.def:
* gm2/examples/map/StoreCoords.mod:
* gm2/examples/map/WriteMap.def:
* gm2/examples/map/WriteMap.mod:
* gm2/examples/map/testch2.mod:
* gm2/examples/map/testchan.mod:
* gm2/examples/map/testcoor.mod:
* gm2/examples/map/old-src/GenMap.mod:
* gm2/examples/map/old-src/MakeMap.mod:
* gm2/examples/map/old-src/MonMap.def:
* gm2/examples/map/old-src/MonMap.mod:
* gm2/examples/map/old-src/testboxes.mod:
* gm2/examples/min/minhello.mod:
* gm2/examples/ncurses/ColorText.def:
* gm2/examples/ncurses/ColorText.mod:
* gm2/examples/ncurses/Makefile.in:
* gm2/examples/ncurses/WindowDevice.def:
* gm2/examples/ncurses/WindowDevice.mod:
* gm2/examples/ncurses/ncurses.def:
* gm2/examples/ncurses/shortc.c:
* gm2/examples/ncurses/shortc.def:
* gm2/examples/ncurses/test.c:
* gm2/examples/ncurses/test.mod:
* gm2/examples/ncurses/testcolor.mod:
* gm2/examples/ncurses/testmove.mod:
* gm2/examples/ncurses/testshort.mod:
* gm2/examples/ncurses/testwin.mod:
* gm2/examples/pthread/fullpth.def:
* gm2/examples/pthread/libcextra.def:
* gm2/examples/pthread/libcwrap.c:
* gm2/examples/pthread/libcwrap.def:
* gm2/examples/pthread/modified_pth.h:
* gm2/examples/pthread/testselect.mod:
* gm2/examples/server/server.mod:
* gm2/examples/svga/DisplayBuffer.def:
* gm2/examples/svga/DisplayBuffer.mod:
* gm2/examples/svga/Matrix3D.def:
* gm2/examples/svga/Matrix3D.mod:
* gm2/examples/svga/Transform.def:
* gm2/examples/svga/Transform.mod:
* gm2/examples/svga/testbox.mod:
* gm2/examples/svga/vga.def:
* gm2/examples/swig/exceptions/Makefile.in:
* gm2/examples/swig/exceptions/tiny.def:
* gm2/examples/swig/exceptions/tiny.mod:
* gm2/examples/swig/full-strlib/testequ.py:
* gm2/examples/swig/strlen/MyStrLib.def:
* gm2/examples/swig/strlen/MyStrLib.mod:
* gm2/examples/swig/strlib/Makefile.in:
* gm2/examples/swig/strlib/MyStrLib.def:
* gm2/examples/swig/strlib/MyStrLib.mod:
* gm2/examples/swig/tiny/Makefile.in:
* gm2/examples/swig/tiny/tiny.def:
* gm2/examples/swig/tiny/tiny.mod:
* gm2/gm2-compiler/CLexBuf.def:
* gm2/gm2-compiler/CLexBuf.mod:
* gm2/gm2-compiler/FifoQueue.def:
* gm2/gm2-compiler/FifoQueue.mod:
* gm2/gm2-compiler/Lists.def:
* gm2/gm2-compiler/Lists.mod:
* gm2/gm2-compiler/M2ALU.def:
* gm2/gm2-compiler/M2ALU.mod:
* gm2/gm2-compiler/M2AsmUtil.def:
* gm2/gm2-compiler/M2AsmUtil.mod:
* gm2/gm2-compiler/M2Base.def:
* gm2/gm2-compiler/M2Base.mod:
* gm2/gm2-compiler/M2BasicBlock.def:
* gm2/gm2-compiler/M2BasicBlock.mod:
* gm2/gm2-compiler/M2Batch.def:
* gm2/gm2-compiler/M2Batch.mod:
* gm2/gm2-compiler/M2Bitset.def:
* gm2/gm2-compiler/M2Bitset.mod:
* gm2/gm2-compiler/M2CaseList.def:
* gm2/gm2-compiler/M2CaseList.mod:
* gm2/gm2-compiler/M2Code.def:
* gm2/gm2-compiler/M2Code.mod:
* gm2/gm2-compiler/M2Comp.def:
* gm2/gm2-compiler/M2Comp.mod:
* gm2/gm2-compiler/M2Configure.def:
* gm2/gm2-compiler/M2Constants.def:
* gm2/gm2-compiler/M2Constants.mod:
* gm2/gm2-compiler/M2Debug.def:
* gm2/gm2-compiler/M2Debug.mod:
* gm2/gm2-compiler/M2Defaults.def:
* gm2/gm2-compiler/M2Defaults.mod:
* gm2/gm2-compiler/M2Depth.def:
* gm2/gm2-compiler/M2Depth.mod:
* gm2/gm2-compiler/M2Entity.def:
* gm2/gm2-compiler/M2Entity.mod:
* gm2/gm2-compiler/M2Error.def:
* gm2/gm2-compiler/M2Error.mod:
* gm2/gm2-compiler/M2EvalSym.def:
* gm2/gm2-compiler/M2FileName.def:
* gm2/gm2-compiler/M2FileName.mod:
* gm2/gm2-compiler/M2GCCDeclare.def:
* gm2/gm2-compiler/M2GCCDeclare.mod:
* gm2/gm2-compiler/M2GenGCC.def:
* gm2/gm2-compiler/M2GenGCC.mod:
* gm2/gm2-compiler/M2Inline.def:
* gm2/gm2-compiler/M2Inline.mod:
* gm2/gm2-compiler/M2Lex.def:
* gm2/gm2-compiler/M2Lex.mod:
* gm2/gm2-compiler/M2LexBuf.def:
* gm2/gm2-compiler/M2LexBuf.mod:
* gm2/gm2-compiler/M2MetaError.def:
* gm2/gm2-compiler/M2MetaError.mod:
* gm2/gm2-compiler/M2Optimize.def:
* gm2/gm2-compiler/M2Optimize.mod:
* gm2/gm2-compiler/M2Options.def:
* gm2/gm2-compiler/M2Options.mod:
* gm2/gm2-compiler/M2Pass.def:
* gm2/gm2-compiler/M2Pass.mod:
* gm2/gm2-compiler/M2Preprocess.def:
* gm2/gm2-compiler/M2Preprocess.mod:
* gm2/gm2-compiler/M2Printf.def:
* gm2/gm2-compiler/M2Printf.mod:
* gm2/gm2-compiler/M2Quads.def:
* gm2/gm2-compiler/M2Quads.mod:
* gm2/gm2-compiler/M2Quiet.def:
* gm2/gm2-compiler/M2Quiet.mod:
* gm2/gm2-compiler/M2Range.def:
* gm2/gm2-compiler/M2Range.mod:
* gm2/gm2-compiler/M2Reserved.def:
* gm2/gm2-compiler/M2Reserved.mod:
* gm2/gm2-compiler/M2Scope.def:
* gm2/gm2-compiler/M2Scope.mod:
* gm2/gm2-compiler/M2Search.def:
* gm2/gm2-compiler/M2Search.mod:
* gm2/gm2-compiler/M2Size.def:
* gm2/gm2-compiler/M2Size.mod:
* gm2/gm2-compiler/M2StackAddress.def:
* gm2/gm2-compiler/M2StackAddress.mod:
* gm2/gm2-compiler/M2StackWord.def:
* gm2/gm2-compiler/M2StackWord.mod:
* gm2/gm2-compiler/M2Students.def:
* gm2/gm2-compiler/M2Students.mod:
* gm2/gm2-compiler/M2SubExp.def:
* gm2/gm2-compiler/M2SubExp.mod:
* gm2/gm2-compiler/M2Swig.def:
* gm2/gm2-compiler/M2Swig.mod:
* gm2/gm2-compiler/M2System.def:
* gm2/gm2-compiler/M2System.mod:
* gm2/gm2-compiler/M2Version.def:
* gm2/gm2-compiler/NameKey.def:
* gm2/gm2-compiler/NameKey.mod:
* gm2/gm2-compiler/P1SymBuild.def:
* gm2/gm2-compiler/P1SymBuild.mod:
* gm2/gm2-compiler/P1SyntaxCheck.def:
* gm2/gm2-compiler/P2Build.def:
* gm2/gm2-compiler/P2SymBuild.def:
* gm2/gm2-compiler/P2SymBuild.mod:
* gm2/gm2-compiler/P3Build.def:
* gm2/gm2-compiler/P3SymBuild.def:
* gm2/gm2-compiler/P3SymBuild.mod:
* gm2/gm2-compiler/PCBuild.def:
* gm2/gm2-compiler/PCSymBuild.def:
* gm2/gm2-compiler/PCSymBuild.mod:
* gm2/gm2-compiler/PHBuild.def:
* gm2/gm2-compiler/Sets.def:
* gm2/gm2-compiler/Sets.mod:
* gm2/gm2-compiler/SymbolConversion.def:
* gm2/gm2-compiler/SymbolConversion.mod:
* gm2/gm2-compiler/SymbolKey.def:
* gm2/gm2-compiler/SymbolKey.mod:
* gm2/gm2-compiler/SymbolTable.def:
* gm2/gm2-compiler/SymbolTable.mod:
* gm2/gm2-compiler/bnflex.def:
* gm2/gm2-compiler/bnflex.mod:
* gm2/gm2-compiler/cflex.def:
* gm2/gm2-compiler/gccgm2.def:
* gm2/gm2-compiler/gm2.mod:
* gm2/gm2-compiler/gm2builtins.def:
* gm2/gm2-compiler/gm2except.def:
* gm2/gm2-compiler/gm2lcc.mod:
* gm2/gm2-compiler/gm2lgen.mod:
* gm2/gm2-compiler/gm2lorder.mod:
* gm2/gm2-compiler/m2flex.def:
* gm2/gm2-compiler/ppg.mod:
* gm2/gm2-harness/Makefile.in:
* gm2/gm2-libiberty/choosetemp.def:
* gm2/gm2-libiberty/pexecute.def:
* gm2/gm2-libs/ASCII.def:
* gm2/gm2-libs/Args.def:
* gm2/gm2-libs/Args.mod:
* gm2/gm2-libs/Assertion.def:
* gm2/gm2-libs/Assertion.mod:
* gm2/gm2-libs/Break.mod:
* gm2/gm2-libs/Builtins.def:
* gm2/gm2-libs/Builtins.mod:
* gm2/gm2-libs/COROUTINES.def:
* gm2/gm2-libs/COROUTINES.mod:
* gm2/gm2-libs/CmdArgs.def:
* gm2/gm2-libs/CmdArgs.mod:
* gm2/gm2-libs/Debug.def:
* gm2/gm2-libs/Debug.mod:
* gm2/gm2-libs/DynamicStrings.def:
* gm2/gm2-libs/DynamicStrings.mod:
* gm2/gm2-libs/Environment.def:
* gm2/gm2-libs/Environment.mod:
* gm2/gm2-libs/FIO.def:
* gm2/gm2-libs/FIO.mod:
* gm2/gm2-libs/FormatStrings.def:
* gm2/gm2-libs/FormatStrings.mod:
* gm2/gm2-libs/FpuIO.def:
* gm2/gm2-libs/FpuIO.mod:
* gm2/gm2-libs/IO.def:
* gm2/gm2-libs/IO.mod:
* gm2/gm2-libs/Indexing.def:
* gm2/gm2-libs/Indexing.mod:
* gm2/gm2-libs/LMathLib0.def:
* gm2/gm2-libs/LMathLib0.mod:
* gm2/gm2-libs/LegacyReal.def:
* gm2/gm2-libs/M2EXCEPTION.mod:
* gm2/gm2-libs/M2RTS.def:
* gm2/gm2-libs/M2RTS.mod:
* gm2/gm2-libs/MathLib0.def:
* gm2/gm2-libs/MathLib0.mod:
* gm2/gm2-libs/MemUtils.def:
* gm2/gm2-libs/MemUtils.mod:
* gm2/gm2-libs/NumberIO.def:
* gm2/gm2-libs/NumberIO.mod:
* gm2/gm2-libs/PushBackInput.def:
* gm2/gm2-libs/PushBackInput.mod:
* gm2/gm2-libs/RTExceptions.def:
* gm2/gm2-libs/RTExceptions.mod:
* gm2/gm2-libs/RTint.def:
* gm2/gm2-libs/RTint.mod:
* gm2/gm2-libs/SArgs.def:
* gm2/gm2-libs/SArgs.mod:
* gm2/gm2-libs/SEnvironment.def:
* gm2/gm2-libs/SEnvironment.mod:
* gm2/gm2-libs/SFIO.def:
* gm2/gm2-libs/SFIO.mod:
* gm2/gm2-libs/SMathLib0.def:
* gm2/gm2-libs/SMathLib0.mod:
* gm2/gm2-libs/SYSTEM.def:
* gm2/gm2-libs/SYSTEM.mod:
* gm2/gm2-libs/Scan.def:
* gm2/gm2-libs/Scan.mod:
* gm2/gm2-libs/Selective.def:
* gm2/gm2-libs/StdIO.def:
* gm2/gm2-libs/StdIO.mod:
* gm2/gm2-libs/Storage.def:
* gm2/gm2-libs/Storage.mod:
* gm2/gm2-libs/StrCase.def:
* gm2/gm2-libs/StrCase.mod:
* gm2/gm2-libs/StrIO.def:
* gm2/gm2-libs/StrIO.mod:
* gm2/gm2-libs/StrLib.def:
* gm2/gm2-libs/StrLib.mod:
* gm2/gm2-libs/StringConvert.def:
* gm2/gm2-libs/StringConvert.mod:
* gm2/gm2-libs/SysExceptions.def:
* gm2/gm2-libs/SysStorage.def:
* gm2/gm2-libs/SysStorage.mod:
* gm2/gm2-libs/TimeString.def:
* gm2/gm2-libs/TimeString.mod:
* gm2/gm2-libs/UnixArgs.def:
* gm2/gm2-libs/cbuiltin.def:
* gm2/gm2-libs/configure.in:
* gm2/gm2-libs/cxxabi.def:
* gm2/gm2-libs/dtoa.def:
* gm2/gm2-libs/errno.def:
* gm2/gm2-libs/gm2-libs-host.h.in:
* gm2/gm2-libs/ldtoa.def:
* gm2/gm2-libs/libc.def:
* gm2/gm2-libs/libm.def:
* gm2/gm2-libs/sckt.def:
* gm2/gm2-libs/termios.def:
* gm2/gm2-libs/wrapc.def:
* gm2/gm2-libs-boot/SYSTEM.def:
* gm2/gm2-libs-ch/Selective.c:
* gm2/gm2-libs-ch/StdIO.c:
* gm2/gm2-libs-ch/Storage.c:
* gm2/gm2-libs-ch/SysExceptions.c:
* gm2/gm2-libs-ch/UnixArgs.c:
* gm2/gm2-libs-ch/choosetemp.c:
* gm2/gm2-libs-ch/dtoa.c:
* gm2/gm2-libs-ch/errno.c:
* gm2/gm2-libs-ch/ldtoa.c:
* gm2/gm2-libs-ch/libc.c:
* gm2/gm2-libs-ch/sckt.c:
* gm2/gm2-libs-ch/target.c:
* gm2/gm2-libs-ch/termios.c:
* gm2/gm2-libs-ch/wrapc.c:
* gm2/gm2-libs-ch/xlibc.c:
* gm2/gm2-libs-coroutines/Debug.def:
* gm2/gm2-libs-coroutines/Debug.mod:
* gm2/gm2-libs-coroutines/Executive.def:
* gm2/gm2-libs-coroutines/Executive.mod:
* gm2/gm2-libs-coroutines/KeyBoardLEDs.c:
* gm2/gm2-libs-coroutines/KeyBoardLEDs.def:
* gm2/gm2-libs-coroutines/SYSTEM.def:
* gm2/gm2-libs-coroutines/SYSTEM.mod:
* gm2/gm2-libs-coroutines/TimerHandler.def:
* gm2/gm2-libs-coroutines/TimerHandler.mod:
* gm2/gm2-libs-iso/COROUTINES.mod:
* gm2/gm2-libs-iso/ChanConsts.h:
* gm2/gm2-libs-iso/ChanConsts.mod:
* gm2/gm2-libs-iso/CharClass.mod:
* gm2/gm2-libs-iso/ClientSocket.def:
* gm2/gm2-libs-iso/ClientSocket.mod:
* gm2/gm2-libs-iso/ComplexMath.mod:
* gm2/gm2-libs-iso/ConvStringLong.def:
* gm2/gm2-libs-iso/ConvStringLong.mod:
* gm2/gm2-libs-iso/ConvStringReal.def:
* gm2/gm2-libs-iso/ConvStringReal.mod:
* gm2/gm2-libs-iso/ConvTypes.mod:
* gm2/gm2-libs-iso/EXCEPTIONS.mod:
* gm2/gm2-libs-iso/ErrnoCategory.c:
* gm2/gm2-libs-iso/ErrnoCategory.def:
* gm2/gm2-libs-iso/GeneralUserExceptions.mod:
* gm2/gm2-libs-iso/IOChan.mod:
* gm2/gm2-libs-iso/IOLink.mod:
* gm2/gm2-libs-iso/IOResult.mod:
* gm2/gm2-libs-iso/LongComplexMath.mod:
* gm2/gm2-libs-iso/LongConv.mod:
* gm2/gm2-libs-iso/LongMath.mod:
* gm2/gm2-libs-iso/LongStr.mod:
* gm2/gm2-libs-iso/LowLong.mod:
* gm2/gm2-libs-iso/LowReal.mod:
* gm2/gm2-libs-iso/LowShort.def:
* gm2/gm2-libs-iso/LowShort.mod:
* gm2/gm2-libs-iso/M2EXCEPTION.mod:
* gm2/gm2-libs-iso/M2RTS.def:
* gm2/gm2-libs-iso/M2RTS.mod:
* gm2/gm2-libs-iso/Processes.mod:
* gm2/gm2-libs-iso/ProgramArgs.mod:
* gm2/gm2-libs-iso/RTdata.def:
* gm2/gm2-libs-iso/RTdata.mod:
* gm2/gm2-libs-iso/RTentity.def:
* gm2/gm2-libs-iso/RTentity.mod:
* gm2/gm2-libs-iso/RTfio.def:
* gm2/gm2-libs-iso/RTfio.mod:
* gm2/gm2-libs-iso/RTgen.def:
* gm2/gm2-libs-iso/RTgen.mod:
* gm2/gm2-libs-iso/RTgenif.def:
* gm2/gm2-libs-iso/RTgenif.mod:
* gm2/gm2-libs-iso/RTio.def:
* gm2/gm2-libs-iso/RTio.mod:
* gm2/gm2-libs-iso/RawIO.mod:
* gm2/gm2-libs-iso/RealConv.mod:
* gm2/gm2-libs-iso/RealMath.mod:
* gm2/gm2-libs-iso/RealStr.mod:
* gm2/gm2-libs-iso/RndFile.mod:
* gm2/gm2-libs-iso/SIOResult.mod:
* gm2/gm2-libs-iso/SLongIO.mod:
* gm2/gm2-libs-iso/SRawIO.mod:
* gm2/gm2-libs-iso/SRealIO.mod:
* gm2/gm2-libs-iso/STextIO.mod:
* gm2/gm2-libs-iso/SWholeIO.mod:
* gm2/gm2-libs-iso/SYSTEM.mod:
* gm2/gm2-libs-iso/Semaphores.mod:
* gm2/gm2-libs-iso/SeqFile.mod:
* gm2/gm2-libs-iso/ServerSocket.def:
* gm2/gm2-libs-iso/ServerSocket.mod:
* gm2/gm2-libs-iso/ShortComplexMath.def:
* gm2/gm2-libs-iso/ShortComplexMath.mod:
* gm2/gm2-libs-iso/SimpleCipher.def:
* gm2/gm2-libs-iso/SimpleCipher.mod:
* gm2/gm2-libs-iso/StdChans.mod:
* gm2/gm2-libs-iso/Storage.mod:
* gm2/gm2-libs-iso/StreamFile.mod:
* gm2/gm2-libs-iso/StringChan.def:
* gm2/gm2-libs-iso/StringChan.mod:
* gm2/gm2-libs-iso/Strings.mod:
* gm2/gm2-libs-iso/SysClock.mod:
* gm2/gm2-libs-iso/TERMINATION.mod:
* gm2/gm2-libs-iso/TermFile.mod:
* gm2/gm2-libs-iso/TextIO.mod:
* gm2/gm2-libs-iso/WholeConv.mod:
* gm2/gm2-libs-iso/WholeIO.mod:
* gm2/gm2-libs-iso/WholeStr.mod:
* gm2/gm2-libs-iso/pth.def:
* gm2/gm2-libs-iso/wrapsock.c:
* gm2/gm2-libs-iso/wrapsock.def:
* gm2/gm2-libs-iso/wraptime.c:
* gm2/gm2-libs-iso/wraptime.def:
* gm2/gm2-libs-min/M2RTS.def:
* gm2/gm2-libs-min/M2RTS.mod:
* gm2/gm2-libs-min/SYSTEM.def:
* gm2/gm2-libs-min/SYSTEM.mod:
* gm2/gm2-libs-min/libc.c:
* gm2/gm2-libs-min/libc.def:
* gm2/gm2-libs-pim/BitBlockOps.def:
* gm2/gm2-libs-pim/BitBlockOps.mod:
* gm2/gm2-libs-pim/BitByteOps.def:
* gm2/gm2-libs-pim/BitByteOps.mod:
* gm2/gm2-libs-pim/BitWordOps.def:
* gm2/gm2-libs-pim/BitWordOps.mod:
* gm2/gm2-libs-pim/BlockOps.def:
* gm2/gm2-libs-pim/BlockOps.mod:
* gm2/gm2-libs-pim/Break.c:
* gm2/gm2-libs-pim/Break.def:
* gm2/gm2-libs-pim/CardinalIO.def:
* gm2/gm2-libs-pim/CardinalIO.mod:
* gm2/gm2-libs-pim/Conversions.def:
* gm2/gm2-libs-pim/Conversions.mod:
* gm2/gm2-libs-pim/DebugTrace.def:
* gm2/gm2-libs-pim/Delay.def:
* gm2/gm2-libs-pim/Delay.mod:
* gm2/gm2-libs-pim/Display.def:
* gm2/gm2-libs-pim/Display.mod:
* gm2/gm2-libs-pim/ErrorCode.def:
* gm2/gm2-libs-pim/ErrorCode.mod:
* gm2/gm2-libs-pim/FileSystem.def:
* gm2/gm2-libs-pim/FileSystem.mod:
* gm2/gm2-libs-pim/FloatingUtilities.def:
* gm2/gm2-libs-pim/FloatingUtilities.mod:
* gm2/gm2-libs-pim/InOut.def:
* gm2/gm2-libs-pim/InOut.mod:
* gm2/gm2-libs-pim/Keyboard.def:
* gm2/gm2-libs-pim/Keyboard.mod:
* gm2/gm2-libs-pim/LongIO.def:
* gm2/gm2-libs-pim/LongIO.mod:
* gm2/gm2-libs-pim/Random.def:
* gm2/gm2-libs-pim/Random.mod:
* gm2/gm2-libs-pim/RealConversions.def:
* gm2/gm2-libs-pim/RealConversions.mod:
* gm2/gm2-libs-pim/RealInOut.def:
* gm2/gm2-libs-pim/RealInOut.mod:
* gm2/gm2-libs-pim/Strings.def:
* gm2/gm2-libs-pim/Strings.mod:
* gm2/gm2-libs-pim/Termbase.def:
* gm2/gm2-libs-pim/Termbase.mod:
* gm2/gm2-libs-pim/Terminal.def:
* gm2/gm2-libs-pim/Terminal.mod:
* gm2/gm2-libs-pim/TimeDate.def:
* gm2/gm2-libs-pim/TimeDate.mod:
* gm2/man/Makefile.in:
* gm2/p2c/Makefile.in:
* gm2/p2c/p2c.h:
* gm2/p2c/p2c-src/Makefile.in:
* gm2/p2c/p2c-src/auto-host.h.in:
* gm2/p2c/p2c-src/configure.in:
* gm2/p2c/p2c-src/include/ansidecl.h:
* gm2/p2c/p2c-src/include/config.h:
* gm2/p2c/p2c-src/include/system.h:
* gm2/p2c/p2c-src/src/Makefile.in:
* gm2/p2c/p2c-src/src/citmods.c:
* gm2/p2c/p2c-src/src/comment.c:
* gm2/p2c/p2c-src/src/decl.c:
* gm2/p2c/p2c-src/src/dir.c:
* gm2/p2c/p2c-src/src/expr.c:
* gm2/p2c/p2c-src/src/funcs.c:
* gm2/p2c/p2c-src/src/hpmods.c:
* gm2/p2c/p2c-src/src/lex.c:
* gm2/p2c/p2c-src/src/loc.p2clib.c:
* gm2/p2c/p2c-src/src/makeproto.c:
* gm2/p2c/p2c-src/src/out.c:
* gm2/p2c/p2c-src/src/p2c-config.h:
* gm2/p2c/p2c-src/src/p2c.h:
* gm2/p2c/p2c-src/src/p2clib.c:
* gm2/p2c/p2c-src/src/parse.c:
* gm2/p2c/p2c-src/src/pexpr.c:
* gm2/p2c/p2c-src/src/stuff.c:
* gm2/p2c/p2c-src/src/trans.c:
* gm2/p2c/p2c-src/src/trans.h:
* gm2/patches/gcc/4.1.2/08.gaius_ipa_type_escape.c:
* gm2/tools-src/array2index.py:
* gm2/tools-src/def2texi.py:
* gm2/tools-src/gensum.py:
* gm2/tools-src/mklink.c:
* gm2/ulm-lib-gm2/processes/CoExpressions.def:
* gm2/ulm-lib-gm2/processes/CoExpressions.mod:
* gm2/ulm-lib-gm2/processes/Processes.def:
* gm2/ulm-lib-gm2/processes/Processes.mod:
* gm2/ulm-lib-gm2/std/ASCII.def:
* gm2/ulm-lib-gm2/std/ASCII.mod:
* gm2/ulm-lib-gm2/std/Archive.def:
* gm2/ulm-lib-gm2/std/Archive.mod:
* gm2/ulm-lib-gm2/std/Arguments.def:
* gm2/ulm-lib-gm2/std/Arguments.mod:
* gm2/ulm-lib-gm2/std/Calendar.def:
* gm2/ulm-lib-gm2/std/Calendar.mod:
* gm2/ulm-lib-gm2/std/CallShell.def:
* gm2/ulm-lib-gm2/std/CallShell.mod:
* gm2/ulm-lib-gm2/std/Clock.def:
* gm2/ulm-lib-gm2/std/Clock.mod:
* gm2/ulm-lib-gm2/std/Conversions.def:
* gm2/ulm-lib-gm2/std/Conversions.mod:
* gm2/ulm-lib-gm2/std/Directories.def:
* gm2/ulm-lib-gm2/std/Directories.mod:
* gm2/ulm-lib-gm2/std/Environment.def:
* gm2/ulm-lib-gm2/std/Environment.mod:
* gm2/ulm-lib-gm2/std/EtcGroup.def:
* gm2/ulm-lib-gm2/std/EtcGroup.mod:
* gm2/ulm-lib-gm2/std/Files.def:
* gm2/ulm-lib-gm2/std/Files.mod:
* gm2/ulm-lib-gm2/std/FtdIO.def:
* gm2/ulm-lib-gm2/std/FtdIO.mod:
* gm2/ulm-lib-gm2/std/Functions.def:
* gm2/ulm-lib-gm2/std/Functions.mod:
* gm2/ulm-lib-gm2/std/GetPass.def:
* gm2/ulm-lib-gm2/std/GetPass.mod:
* gm2/ulm-lib-gm2/std/InOut.def:
* gm2/ulm-lib-gm2/std/InOut.mod:
* gm2/ulm-lib-gm2/std/M2EXCEPTION.mod:
* gm2/ulm-lib-gm2/std/M2RTS.mod:
* gm2/ulm-lib-gm2/std/MathLib.def:
* gm2/ulm-lib-gm2/std/MathLib.mod:
* gm2/ulm-lib-gm2/std/Passwd.def:
* gm2/ulm-lib-gm2/std/Passwd.mod:
* gm2/ulm-lib-gm2/std/PipeIO.def:
* gm2/ulm-lib-gm2/std/PipeIO.mod:
* gm2/ulm-lib-gm2/std/Plot.def:
* gm2/ulm-lib-gm2/std/Plot.mod:
* gm2/ulm-lib-gm2/std/RTErrors.def:
* gm2/ulm-lib-gm2/std/RTErrors.mod:
* gm2/ulm-lib-gm2/std/RTExceptions.mod:
* gm2/ulm-lib-gm2/std/RandomGenerator.def:
* gm2/ulm-lib-gm2/std/RandomGenerator.mod:
* gm2/ulm-lib-gm2/std/ReadIntCard.def:
* gm2/ulm-lib-gm2/std/ReadIntCard.mod:
* gm2/ulm-lib-gm2/std/RealConv.def:
* gm2/ulm-lib-gm2/std/RealConv.mod:
* gm2/ulm-lib-gm2/std/RealInOut.def:
* gm2/ulm-lib-gm2/std/RealInOut.mod:
* gm2/ulm-lib-gm2/std/ScanPwfile.def:
* gm2/ulm-lib-gm2/std/ScanPwfile.mod:
* gm2/ulm-lib-gm2/std/StdFuncs.def:
* gm2/ulm-lib-gm2/std/StdFuncs.mod:
* gm2/ulm-lib-gm2/std/StdIO.def:
* gm2/ulm-lib-gm2/std/StdIO.mod:
* gm2/ulm-lib-gm2/std/Storage.def:
* gm2/ulm-lib-gm2/std/Storage.mod:
* gm2/ulm-lib-gm2/std/StrSpec.def:
* gm2/ulm-lib-gm2/std/StrSpec.mod:
* gm2/ulm-lib-gm2/std/StrToNum.def:
* gm2/ulm-lib-gm2/std/StrToNum.mod:
* gm2/ulm-lib-gm2/std/StrToReal.def:
* gm2/ulm-lib-gm2/std/StrToReal.mod:
* gm2/ulm-lib-gm2/std/Strings.def:
* gm2/ulm-lib-gm2/std/Strings.mod:
* gm2/ulm-lib-gm2/std/SysConf.def:
* gm2/ulm-lib-gm2/std/SysConf.mod:
* gm2/ulm-lib-gm2/std/SysPerror.def:
* gm2/ulm-lib-gm2/std/SysPerror.mod:
* gm2/ulm-lib-gm2/std/Terminal.def:
* gm2/ulm-lib-gm2/std/Terminal.mod:
* gm2/ulm-lib-gm2/std/TimeIO.def:
* gm2/ulm-lib-gm2/std/TimeIO.mod:
* gm2/ulm-lib-gm2/sys/Errno.def:
* gm2/ulm-lib-gm2/sys/Errno.mod:
* gm2/ulm-lib-gm2/sys/SYSTEM.def:
* gm2/ulm-lib-gm2/sys/Sys.def:
* gm2/ulm-lib-gm2/sys/Sys.mod:
* gm2/ulm-lib-gm2/sys/SysAccess.def:
* gm2/ulm-lib-gm2/sys/SysAccess.mod:
* gm2/ulm-lib-gm2/sys/SysAlarm.def:
* gm2/ulm-lib-gm2/sys/SysAlarm.mod:
* gm2/ulm-lib-gm2/sys/SysBreak.def:
* gm2/ulm-lib-gm2/sys/SysBreak.mod:
* gm2/ulm-lib-gm2/sys/SysClose.def:
* gm2/ulm-lib-gm2/sys/SysClose.mod:
* gm2/ulm-lib-gm2/sys/SysCreat.def:
* gm2/ulm-lib-gm2/sys/SysCreat.mod:
* gm2/ulm-lib-gm2/sys/SysDup.def:
* gm2/ulm-lib-gm2/sys/SysDup.mod:
* gm2/ulm-lib-gm2/sys/SysExec.def:
* gm2/ulm-lib-gm2/sys/SysExec.mod:
* gm2/ulm-lib-gm2/sys/SysExit.def:
* gm2/ulm-lib-gm2/sys/SysExit.mod:
* gm2/ulm-lib-gm2/sys/SysFcntl.def:
* gm2/ulm-lib-gm2/sys/SysFcntl.mod:
* gm2/ulm-lib-gm2/sys/SysFork.def:
* gm2/ulm-lib-gm2/sys/SysFork.mod:
* gm2/ulm-lib-gm2/sys/SysGetpid.def:
* gm2/ulm-lib-gm2/sys/SysGetpid.mod:
* gm2/ulm-lib-gm2/sys/SysGetuid.def:
* gm2/ulm-lib-gm2/sys/SysGetuid.mod:
* gm2/ulm-lib-gm2/sys/SysIoctl.def:
* gm2/ulm-lib-gm2/sys/SysIoctl.mod:
* gm2/ulm-lib-gm2/sys/SysKill.def:
* gm2/ulm-lib-gm2/sys/SysKill.mod:
* gm2/ulm-lib-gm2/sys/SysLink.def:
* gm2/ulm-lib-gm2/sys/SysLink.mod:
* gm2/ulm-lib-gm2/sys/SysLocations.def:
* gm2/ulm-lib-gm2/sys/SysLocations.mod:
* gm2/ulm-lib-gm2/sys/SysLseek.def:
* gm2/ulm-lib-gm2/sys/SysLseek.mod:
* gm2/ulm-lib-gm2/sys/SysOpen.def:
* gm2/ulm-lib-gm2/sys/SysOpen.mod:
* gm2/ulm-lib-gm2/sys/SysPanic.def:
* gm2/ulm-lib-gm2/sys/SysPanic.mod:
* gm2/ulm-lib-gm2/sys/SysPause.def:
* gm2/ulm-lib-gm2/sys/SysPause.mod:
* gm2/ulm-lib-gm2/sys/SysPipe.def:
* gm2/ulm-lib-gm2/sys/SysPipe.mod:
* gm2/ulm-lib-gm2/sys/SysRead.def:
* gm2/ulm-lib-gm2/sys/SysRead.mod:
* gm2/ulm-lib-gm2/sys/SysSetuid.def:
* gm2/ulm-lib-gm2/sys/SysSetuid.mod:
* gm2/ulm-lib-gm2/sys/SysSignal.def:
* gm2/ulm-lib-gm2/sys/SysSignal.mod:
* gm2/ulm-lib-gm2/sys/SysStat.def:
* gm2/ulm-lib-gm2/sys/SysStat.mod:
* gm2/ulm-lib-gm2/sys/SysTermIO.def:
* gm2/ulm-lib-gm2/sys/SysTermIO.mod:
* gm2/ulm-lib-gm2/sys/SysTime.def:
* gm2/ulm-lib-gm2/sys/SysTime.mod:
* gm2/ulm-lib-gm2/sys/SysUnlink.def:
* gm2/ulm-lib-gm2/sys/SysUnlink.mod:
* gm2/ulm-lib-gm2/sys/SysWait.def:
* gm2/ulm-lib-gm2/sys/SysWait.mod:
* gm2/ulm-lib-gm2/sys/SysWrite.def:
* gm2/ulm-lib-gm2/sys/SysWrite.mod:
* gm2/ulm-lib-gm2/sys/SystemTypes.def:
* gm2/ulm-lib-gm2/sys/SystemTypes.mod:
* gm2/ulm-lib-gm2/sys/UnixString.def:
* gm2/ulm-lib-gm2/sys/UnixString.mod:
* gm2/ulm-lib-gm2/sys/test.mod:
* gm2/www/Makefile.in:
</pre>
</div>



<div>
<hr />

<a name="rev1.10"></a>


Revision <strong>1.10</strong> -

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?revision=1.10&amp;root=gm2&amp;view=markup">view</a>)

(<a href="/viewvc/*checkout*/gm2/examples/ncurses/ColorText.mod?revision=1.10&amp;root=gm2">download</a>)

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?annotate=1.10&amp;root=gm2">annotate</a>)



- <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.10&amp;view=log">[select for diffs]</a>




<br />

<em>Wed Sep 29 05:32:03 2010 UTC</em> (5 months ago) by <em>gaius</em>


<br />Branch:

<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?view=log&amp;root=gm2&amp;pathrev=MAIN"><strong>MAIN</strong></a>







<br />Changes since <strong>1.9: +1 -1 lines</strong>







<br />Diff to <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.9&amp;r2=1.10">previous 1.9</a>










<pre class="vc_log">* gm2/Make-lang.in:  changed license to v3 of GPL and LGPL for
  all the following files.
</pre>
</div>



<div>
<hr />

<a name="rev1.9"></a>


Revision <strong>1.9</strong> -

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?revision=1.9&amp;root=gm2&amp;view=markup">view</a>)

(<a href="/viewvc/*checkout*/gm2/examples/ncurses/ColorText.mod?revision=1.9&amp;root=gm2">download</a>)

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?annotate=1.9&amp;root=gm2">annotate</a>)



- <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.9&amp;view=log">[select for diffs]</a>




<br />

<em>Tue Sep 21 10:02:10 2010 UTC</em> (5 months, 1 week ago) by <em>gaius</em>


<br />Branch:

<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?view=log&amp;root=gm2&amp;pathrev=MAIN"><strong>MAIN</strong></a>







<br />Changes since <strong>1.8: +1 -1 lines</strong>







<br />Diff to <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.8&amp;r2=1.9">previous 1.8</a>










<pre class="vc_log">* gm2/www/index.ms:  updated information for the 0.98 release.
* gm2/gm2-compiler/M2Quads.mod:  fixed bug reported by
  DragiÅ¡a DuriÄ (dragisa-duric/testcase16/pass/TermIO.mod)
  and also DragiÅ¡a DuriÄ
  (dragisa-duric/testcase15/pass/testcase15.mod).
* gm2/gm2-compiler/P2SymBuild.def:  (New procedure) SkipConst.
* gm2/gm2-compiler/P2SymBuild.mod:  (New procedure) SkipConst
  implemented.
* gm2/gm2-compiler/PCSymBuild.mod:  call FixupConstExpr if a
  constant is assigned to a procedure.
* gm2/Make-lang.in:  fixed dates for the following files.
* gm2/Makefile.in:
* gm2/configure.in:
* gm2/gm2-common.c:
* gm2/gm2-common.h:
* gm2/gm2-lang.c:
* gm2/gm2-lang.h:
* gm2/gm2-tree.def:
* gm2/gm2-tree.h:
* gm2/gm2builtins.c:
* gm2/gm2config.h.in:
* gm2/gm2except.c:
* gm2/gm2spec.c:
* gm2/gm2version.h:
* gm2/lang-options.h:
* gm2/m2pp.c:
* gm2/m2pp.h:
* gm2/bnf/gm2l.bnf:
* gm2/bnf/gm2m.bnf:
* gm2/bnf/h2def.bnf:
* gm2/bnf/m2-2.bnf:
* gm2/bnf/m2-3.bnf:
* gm2/bnf/m2-h.bnf:
* gm2/bnf/m2.bnf:
* gm2/examples/callingC/Makefile.in:
* gm2/examples/callingC/hello.mod:
* gm2/examples/callingC/libprintf.def:
* gm2/examples/cplusplus/cppcatchm2/cpp.def:
* gm2/examples/cplusplus/cppcatchm2/m2.def:
* gm2/examples/cplusplus/cppcatchm2/m2.mod:
* gm2/examples/cplusplus/m2catchcpp/cpp.def:
* gm2/examples/cplusplus/m2catchcpp/m2.mod:
* gm2/examples/cpp/hello.mod:
* gm2/examples/cppDef/a.def:
* gm2/examples/cppDef/a.mod:
* gm2/examples/cppDef/b.mod:
* gm2/examples/executive/testexecutive.mod:
* gm2/examples/executive/testtime.mod:
* gm2/examples/executive/winexec.mod:
* gm2/examples/gravity/deviceGnuPic.def:
* gm2/examples/gravity/deviceGnuPic.mod:
* gm2/examples/gravity/gsl.def:
* gm2/examples/gravity/test.mod:
* gm2/examples/gravity/twoDsim.def:
* gm2/examples/hello/Makefile.in:
* gm2/examples/hello/hello.mod:
* gm2/examples/iso/files/rot13.mod:
* gm2/examples/iso/hello/hello.mod:
* gm2/examples/iso/socket/client.mod:
* gm2/examples/map/AdvMap.def:
* gm2/examples/map/AdvMap.mod:
* gm2/examples/map/BoxMap.def:
* gm2/examples/map/BoxMap.mod:
* gm2/examples/map/Chance.def:
* gm2/examples/map/Chance.mod:
* gm2/examples/map/Find.def:
* gm2/examples/map/Find.mod:
* gm2/examples/map/Geometry.def:
* gm2/examples/map/Geometry.mod:
* gm2/examples/map/MakeBoxes.def:
* gm2/examples/map/MakeBoxes.mod:
* gm2/examples/map/Makefile.in:
* gm2/examples/map/Map.mod:
* gm2/examples/map/RoomMap.def:
* gm2/examples/map/RoomMap.mod:
* gm2/examples/map/Semantic.mod:
* gm2/examples/map/StoreCoord.def:
* gm2/examples/map/StoreCoord.mod:
* gm2/examples/map/StoreCoords.def:
* gm2/examples/map/StoreCoords.mod:
* gm2/examples/map/WriteMap.def:
* gm2/examples/map/WriteMap.mod:
* gm2/examples/map/testch2.mod:
* gm2/examples/map/testchan.mod:
* gm2/examples/map/testcoor.mod:
* gm2/examples/map/old-src/GenMap.mod:
* gm2/examples/map/old-src/MakeMap.mod:
* gm2/examples/map/old-src/MonMap.def:
* gm2/examples/map/old-src/MonMap.mod:
* gm2/examples/map/old-src/testboxes.mod:
* gm2/examples/min/minhello.mod:
* gm2/examples/ncurses/ColorText.def:
* gm2/examples/ncurses/ColorText.mod:
* gm2/examples/ncurses/Makefile.in:
* gm2/examples/ncurses/WindowDevice.def:
* gm2/examples/ncurses/WindowDevice.mod:
* gm2/examples/ncurses/ncurses.def:
* gm2/examples/ncurses/shortc.c:
* gm2/examples/ncurses/shortc.def:
* gm2/examples/ncurses/test.c:
* gm2/examples/ncurses/test.mod:
* gm2/examples/ncurses/testcolor.mod:
* gm2/examples/ncurses/testmove.mod:
* gm2/examples/ncurses/testshort.mod:
* gm2/examples/ncurses/testwin.mod:
* gm2/examples/pthread/fullpth.def:
* gm2/examples/pthread/libcextra.def:
* gm2/examples/pthread/libcwrap.c:
* gm2/examples/pthread/libcwrap.def:
* gm2/examples/pthread/testselect.mod:
* gm2/examples/server/server.mod:
* gm2/examples/svga/DisplayBuffer.def:
* gm2/examples/svga/DisplayBuffer.mod:
* gm2/examples/svga/Matrix3D.def:
* gm2/examples/svga/Matrix3D.mod:
* gm2/examples/svga/Transform.def:
* gm2/examples/svga/Transform.mod:
* gm2/examples/svga/testbox.mod:
* gm2/examples/svga/vga.def:
* gm2/examples/swig/exceptions/tiny.def:
* gm2/examples/swig/exceptions/tiny.mod:
* gm2/examples/swig/strlen/MyStrLib.def:
* gm2/examples/swig/strlen/MyStrLib.mod:
* gm2/examples/swig/strlib/MyStrLib.def:
* gm2/examples/swig/strlib/MyStrLib.mod:
* gm2/examples/swig/tiny/tiny.def:
* gm2/examples/swig/tiny/tiny.mod:
* gm2/gm2-compiler/CLexBuf.def:
* gm2/gm2-compiler/CLexBuf.mod:
* gm2/gm2-compiler/FifoQueue.def:
* gm2/gm2-compiler/FifoQueue.mod:
* gm2/gm2-compiler/Lists.def:
* gm2/gm2-compiler/Lists.mod:
* gm2/gm2-compiler/M2ALU.def:
* gm2/gm2-compiler/M2ALU.mod:
* gm2/gm2-compiler/M2AsmUtil.def:
* gm2/gm2-compiler/M2AsmUtil.mod:
* gm2/gm2-compiler/M2Base.def:
* gm2/gm2-compiler/M2Base.mod:
* gm2/gm2-compiler/M2BasicBlock.def:
* gm2/gm2-compiler/M2BasicBlock.mod:
* gm2/gm2-compiler/M2Batch.def:
* gm2/gm2-compiler/M2Batch.mod:
* gm2/gm2-compiler/M2Bitset.def:
* gm2/gm2-compiler/M2Bitset.mod:
* gm2/gm2-compiler/M2CaseList.def:
* gm2/gm2-compiler/M2CaseList.mod:
* gm2/gm2-compiler/M2Code.def:
* gm2/gm2-compiler/M2Code.mod:
* gm2/gm2-compiler/M2Comp.def:
* gm2/gm2-compiler/M2Comp.mod:
* gm2/gm2-compiler/M2Configure.def:
* gm2/gm2-compiler/M2Configure.mod:
* gm2/gm2-compiler/M2Constants.def:
* gm2/gm2-compiler/M2Constants.mod:
* gm2/gm2-compiler/M2Debug.def:
* gm2/gm2-compiler/M2Debug.mod:
* gm2/gm2-compiler/M2Defaults.def:
* gm2/gm2-compiler/M2Defaults.mod:
* gm2/gm2-compiler/M2Depth.def:
* gm2/gm2-compiler/M2Depth.mod:
* gm2/gm2-compiler/M2Entity.def:
* gm2/gm2-compiler/M2Entity.mod:
* gm2/gm2-compiler/M2Error.def:
* gm2/gm2-compiler/M2Error.mod:
* gm2/gm2-compiler/M2EvalSym.def:
* gm2/gm2-compiler/M2FileName.def:
* gm2/gm2-compiler/M2FileName.mod:
* gm2/gm2-compiler/M2GCCDeclare.def:
* gm2/gm2-compiler/M2GCCDeclare.mod:
* gm2/gm2-compiler/M2GenGCC.def:
* gm2/gm2-compiler/M2Inline.def:
* gm2/gm2-compiler/M2Inline.mod:
* gm2/gm2-compiler/M2Lex.def:
* gm2/gm2-compiler/M2Lex.mod:
* gm2/gm2-compiler/M2LexBuf.def:
* gm2/gm2-compiler/M2LexBuf.mod:
* gm2/gm2-compiler/M2MetaError.def:
* gm2/gm2-compiler/M2MetaError.mod:
* gm2/gm2-compiler/M2Optimize.def:
* gm2/gm2-compiler/M2Optimize.mod:
* gm2/gm2-compiler/M2Options.def:
* gm2/gm2-compiler/M2Options.mod:
* gm2/gm2-compiler/M2Pass.mod:
* gm2/gm2-compiler/M2Preprocess.def:
* gm2/gm2-compiler/M2Preprocess.mod:
* gm2/gm2-compiler/M2Printf.def:
* gm2/gm2-compiler/M2Printf.mod:
* gm2/gm2-compiler/M2Quads.def:
* gm2/gm2-compiler/M2Quads.mod:
* gm2/gm2-compiler/M2Quiet.def:
* gm2/gm2-compiler/M2Quiet.mod:
* gm2/gm2-compiler/M2Range.def:
* gm2/gm2-compiler/M2Range.mod:
* gm2/gm2-compiler/M2Reserved.def:
* gm2/gm2-compiler/M2Reserved.mod:
* gm2/gm2-compiler/M2Scope.def:
* gm2/gm2-compiler/M2Scope.mod:
* gm2/gm2-compiler/M2Search.def:
* gm2/gm2-compiler/M2Search.mod:
* gm2/gm2-compiler/M2Size.def:
* gm2/gm2-compiler/M2Size.mod:
* gm2/gm2-compiler/M2StackAddress.def:
* gm2/gm2-compiler/M2StackAddress.mod:
* gm2/gm2-compiler/M2StackWord.def:
* gm2/gm2-compiler/M2StackWord.mod:
* gm2/gm2-compiler/M2Students.def:
* gm2/gm2-compiler/M2Students.mod:
* gm2/gm2-compiler/M2SubExp.def:
* gm2/gm2-compiler/M2SubExp.mod:
* gm2/gm2-compiler/M2Swig.def:
* gm2/gm2-compiler/M2Swig.mod:
* gm2/gm2-compiler/M2System.def:
* gm2/gm2-compiler/M2Version.def:
* gm2/gm2-compiler/NameKey.def:
* gm2/gm2-compiler/NameKey.mod:
* gm2/gm2-compiler/P1SymBuild.def:
* gm2/gm2-compiler/P1SymBuild.mod:
* gm2/gm2-compiler/P1SyntaxCheck.def:
* gm2/gm2-compiler/P2Build.def:
* gm2/gm2-compiler/P2SymBuild.def:
* gm2/gm2-compiler/P2SymBuild.mod:
* gm2/gm2-compiler/P3Build.def:
* gm2/gm2-compiler/P3SymBuild.def:
* gm2/gm2-compiler/P3SymBuild.mod:
* gm2/gm2-compiler/PCBuild.def:
* gm2/gm2-compiler/PCSymBuild.def:
* gm2/gm2-compiler/PCSymBuild.mod:
* gm2/gm2-compiler/PHBuild.def:
* gm2/gm2-compiler/Sets.def:
* gm2/gm2-compiler/Sets.mod:
* gm2/gm2-compiler/SymbolConversion.def:
* gm2/gm2-compiler/SymbolConversion.mod:
* gm2/gm2-compiler/SymbolKey.def:
* gm2/gm2-compiler/SymbolKey.mod:
* gm2/gm2-compiler/SymbolTable.def:
* gm2/gm2-compiler/SymbolTable.mod:
* gm2/gm2-compiler/bnflex.def:
* gm2/gm2-compiler/bnflex.mod:
* gm2/gm2-compiler/cflex.def:
* gm2/gm2-compiler/gccgm2.def:
* gm2/gm2-compiler/gm2.mod:
* gm2/gm2-compiler/gm2builtins.def:
* gm2/gm2-compiler/gm2except.def:
* gm2/gm2-compiler/gm2lcc.mod:
* gm2/gm2-compiler/gm2lgen.mod:
* gm2/gm2-compiler/gm2lorder.mod:
* gm2/gm2-compiler/m2flex.def:
* gm2/gm2-compiler/ppg.mod:
* gm2/gm2-libiberty/choosetemp.def:
* gm2/gm2-libiberty/pexecute.def:
* gm2/gm2-libs/ASCII.def:
* gm2/gm2-libs/ASCII.mod:
* gm2/gm2-libs/Args.def:
* gm2/gm2-libs/Args.mod:
* gm2/gm2-libs/Assertion.def:
* gm2/gm2-libs/Assertion.mod:
* gm2/gm2-libs/Break.def:
* gm2/gm2-libs/Break.mod:
* gm2/gm2-libs/Builtins.def:
* gm2/gm2-libs/COROUTINES.def:
* gm2/gm2-libs/COROUTINES.mod:
* gm2/gm2-libs/CmdArgs.def:
* gm2/gm2-libs/CmdArgs.mod:
* gm2/gm2-libs/Debug.def:
* gm2/gm2-libs/Debug.mod:
* gm2/gm2-libs/DynamicStrings.def:
* gm2/gm2-libs/DynamicStrings.mod:
* gm2/gm2-libs/Environment.def:
* gm2/gm2-libs/Environment.mod:
* gm2/gm2-libs/FIO.def:
* gm2/gm2-libs/FIO.mod:
* gm2/gm2-libs/FormatStrings.def:
* gm2/gm2-libs/FormatStrings.mod:
* gm2/gm2-libs/FpuIO.def:
* gm2/gm2-libs/FpuIO.mod:
* gm2/gm2-libs/Indexing.def:
* gm2/gm2-libs/Indexing.mod:
* gm2/gm2-libs/LMathLib0.def:
* gm2/gm2-libs/LMathLib0.mod:
* gm2/gm2-libs/LegacyReal.def:
* gm2/gm2-libs/LegacyReal.mod:
* gm2/gm2-libs/M2EXCEPTION.mod:
* gm2/gm2-libs/M2RTS.def:
* gm2/gm2-libs/M2RTS.mod:
* gm2/gm2-libs/MathLib0.def:
* gm2/gm2-libs/MathLib0.mod:
* gm2/gm2-libs/MemUtils.def:
* gm2/gm2-libs/MemUtils.mod:
* gm2/gm2-libs/NumberIO.def:
* gm2/gm2-libs/NumberIO.mod:
* gm2/gm2-libs/PushBackInput.def:
* gm2/gm2-libs/PushBackInput.mod:
* gm2/gm2-libs/RTExceptions.def:
* gm2/gm2-libs/RTExceptions.mod:
* gm2/gm2-libs/RTint.def:
* gm2/gm2-libs/RTint.mod:
* gm2/gm2-libs/SArgs.def:
* gm2/gm2-libs/SArgs.mod:
* gm2/gm2-libs/SEnvironment.def:
* gm2/gm2-libs/SEnvironment.mod:
* gm2/gm2-libs/SFIO.def:
* gm2/gm2-libs/SFIO.mod:
* gm2/gm2-libs/SMathLib0.def:
* gm2/gm2-libs/SMathLib0.mod:
* gm2/gm2-libs/SYSTEM.def:
* gm2/gm2-libs/SYSTEM.mod:
* gm2/gm2-libs/Scan.def:
* gm2/gm2-libs/Scan.mod:
* gm2/gm2-libs/Selective.def:
* gm2/gm2-libs/StdIO.def:
* gm2/gm2-libs/StdIO.mod:
* gm2/gm2-libs/Storage.def:
* gm2/gm2-libs/Storage.mod:
* gm2/gm2-libs/StrCase.def:
* gm2/gm2-libs/StrCase.mod:
* gm2/gm2-libs/StrIO.def:
* gm2/gm2-libs/StrIO.mod:
* gm2/gm2-libs/StrLib.def:
* gm2/gm2-libs/StrLib.mod:
* gm2/gm2-libs/SysExceptions.def:
* gm2/gm2-libs/SysStorage.def:
* gm2/gm2-libs/SysStorage.mod:
* gm2/gm2-libs/TimeString.def:
* gm2/gm2-libs/TimeString.mod:
* gm2/gm2-libs/UnixArgs.def:
* gm2/gm2-libs/cxxabi.def:
* gm2/gm2-libs/dtoa.def:
* gm2/gm2-libs/errno.def:
* gm2/gm2-libs/gm2-libs-host.h.in:
* gm2/gm2-libs/ldtoa.def:
* gm2/gm2-libs/libc.def:
* gm2/gm2-libs/libm.def:
* gm2/gm2-libs/sckt.def:
* gm2/gm2-libs/termios.def:
* gm2/gm2-libs/wrapc.def:
* gm2/gm2-libs-boot/SYSTEM.def:
* gm2/gm2-libs-ch/Selective.c:
* gm2/gm2-libs-ch/StdIO.c:
* gm2/gm2-libs-ch/Storage.c:
* gm2/gm2-libs-ch/SysExceptions.c:
* gm2/gm2-libs-ch/UnixArgs.c:
* gm2/gm2-libs-ch/choosetemp.c:
* gm2/gm2-libs-ch/dtoa.c:
* gm2/gm2-libs-ch/errno.c:
* gm2/gm2-libs-ch/ldtoa.c:
* gm2/gm2-libs-ch/libc.c:
* gm2/gm2-libs-ch/sckt.c:
* gm2/gm2-libs-ch/wrapc.c:
* gm2/gm2-libs-ch/xlibc.c:
* gm2/gm2-libs-coroutines/Debug.def:
* gm2/gm2-libs-coroutines/Debug.mod:
* gm2/gm2-libs-coroutines/Executive.def:
* gm2/gm2-libs-coroutines/Executive.mod:
* gm2/gm2-libs-coroutines/KeyBoardLEDs.c:
* gm2/gm2-libs-coroutines/KeyBoardLEDs.def:
* gm2/gm2-libs-coroutines/SYSTEM.def:
* gm2/gm2-libs-coroutines/SYSTEM.mod:
* gm2/gm2-libs-coroutines/TimerHandler.def:
* gm2/gm2-libs-coroutines/TimerHandler.mod:
* gm2/gm2-libs-iso/ChanConsts.h:
* gm2/gm2-libs-iso/ChanConsts.mod:
* gm2/gm2-libs-iso/CharClass.mod:
* gm2/gm2-libs-iso/ClientSocket.def:
* gm2/gm2-libs-iso/ClientSocket.mod:
* gm2/gm2-libs-iso/ComplexMath.mod:
* gm2/gm2-libs-iso/ConvStringLong.def:
* gm2/gm2-libs-iso/ConvStringLong.mod:
* gm2/gm2-libs-iso/ConvStringReal.def:
* gm2/gm2-libs-iso/ConvStringReal.mod:
* gm2/gm2-libs-iso/ConvTypes.mod:
* gm2/gm2-libs-iso/EXCEPTIONS.mod:
* gm2/gm2-libs-iso/ErrnoCategory.c:
* gm2/gm2-libs-iso/ErrnoCategory.def:
* gm2/gm2-libs-iso/IOChan.mod:
* gm2/gm2-libs-iso/IOConsts.mod:
* gm2/gm2-libs-iso/IOLink.mod:
* gm2/gm2-libs-iso/IOResult.mod:
* gm2/gm2-libs-iso/LongComplexMath.mod:
* gm2/gm2-libs-iso/LongConv.mod:
* gm2/gm2-libs-iso/LongMath.mod:
* gm2/gm2-libs-iso/LongStr.mod:
* gm2/gm2-libs-iso/M2RTS.def:
* gm2/gm2-libs-iso/M2RTS.mod:
* gm2/gm2-libs-iso/ProgramArgs.mod:
* gm2/gm2-libs-iso/RTdata.def:
* gm2/gm2-libs-iso/RTdata.mod:
* gm2/gm2-libs-iso/RTentity.def:
* gm2/gm2-libs-iso/RTentity.mod:
* gm2/gm2-libs-iso/RTfio.def:
* gm2/gm2-libs-iso/RTfio.mod:
* gm2/gm2-libs-iso/RTgen.def:
* gm2/gm2-libs-iso/RTgen.mod:
* gm2/gm2-libs-iso/RTgenif.def:
* gm2/gm2-libs-iso/RTgenif.mod:
* gm2/gm2-libs-iso/RTio.def:
* gm2/gm2-libs-iso/RTio.mod:
* gm2/gm2-libs-iso/RawIO.mod:
* gm2/gm2-libs-iso/RealConv.mod:
* gm2/gm2-libs-iso/RealMath.mod:
* gm2/gm2-libs-iso/RealStr.mod:
* gm2/gm2-libs-iso/RndFile.mod:
* gm2/gm2-libs-iso/SIOResult.mod:
* gm2/gm2-libs-iso/SLongIO.mod:
* gm2/gm2-libs-iso/SRawIO.mod:
* gm2/gm2-libs-iso/SRealIO.mod:
* gm2/gm2-libs-iso/STextIO.mod:
* gm2/gm2-libs-iso/SWholeIO.mod:
* gm2/gm2-libs-iso/SYSTEM.mod:
* gm2/gm2-libs-iso/SeqFile.mod:
* gm2/gm2-libs-iso/ServerSocket.def:
* gm2/gm2-libs-iso/ServerSocket.mod:
* gm2/gm2-libs-iso/ShortComplexMath.mod:
* gm2/gm2-libs-iso/SimpleCipher.def:
* gm2/gm2-libs-iso/SimpleCipher.mod:
* gm2/gm2-libs-iso/StdChans.mod:
* gm2/gm2-libs-iso/Storage.mod:
* gm2/gm2-libs-iso/StreamFile.mod:
* gm2/gm2-libs-iso/StringChan.def:
* gm2/gm2-libs-iso/StringChan.mod:
* gm2/gm2-libs-iso/Strings.mod:
* gm2/gm2-libs-iso/SysClock.mod:
* gm2/gm2-libs-iso/TERMINATION.mod:
* gm2/gm2-libs-iso/TermFile.mod:
* gm2/gm2-libs-iso/TextIO.mod:
* gm2/gm2-libs-iso/WholeConv.mod:
* gm2/gm2-libs-iso/WholeIO.mod:
* gm2/gm2-libs-iso/WholeStr.mod:
* gm2/gm2-libs-iso/wrapsock.c:
* gm2/gm2-libs-iso/wrapsock.def:
* gm2/gm2-libs-iso/wraptime.c:
* gm2/gm2-libs-iso/wraptime.def:
* gm2/gm2-libs-min/M2RTS.def:
* gm2/gm2-libs-min/M2RTS.mod:
* gm2/gm2-libs-min/SYSTEM.def:
* gm2/gm2-libs-min/SYSTEM.mod:
* gm2/gm2-libs-min/libc.def:
* gm2/gm2-libs-pim/BitBlockOps.def:
* gm2/gm2-libs-pim/BitBlockOps.mod:
* gm2/gm2-libs-pim/BitByteOps.def:
* gm2/gm2-libs-pim/BitByteOps.mod:
* gm2/gm2-libs-pim/BitWordOps.def:
* gm2/gm2-libs-pim/BitWordOps.mod:
* gm2/gm2-libs-pim/BlockOps.def:
* gm2/gm2-libs-pim/BlockOps.mod:
* gm2/gm2-libs-pim/Break.c:
* gm2/gm2-libs-pim/Break.def:
* gm2/gm2-libs-pim/CardinalIO.def:
* gm2/gm2-libs-pim/CardinalIO.mod:
* gm2/gm2-libs-pim/Conversions.def:
* gm2/gm2-libs-pim/Conversions.mod:
* gm2/gm2-libs-pim/DebugPMD.def:
* gm2/gm2-libs-pim/DebugPMD.mod:
* gm2/gm2-libs-pim/DebugTrace.def:
* gm2/gm2-libs-pim/DebugTrace.mod:
* gm2/gm2-libs-pim/Delay.def:
* gm2/gm2-libs-pim/Delay.mod:
* gm2/gm2-libs-pim/Display.def:
* gm2/gm2-libs-pim/Display.mod:
* gm2/gm2-libs-pim/ErrorCode.def:
* gm2/gm2-libs-pim/ErrorCode.mod:
* gm2/gm2-libs-pim/FileSystem.def:
* gm2/gm2-libs-pim/FileSystem.mod:
* gm2/gm2-libs-pim/FloatingUtilities.def:
* gm2/gm2-libs-pim/FloatingUtilities.mod:
* gm2/gm2-libs-pim/InOut.def:
* gm2/gm2-libs-pim/InOut.mod:
* gm2/gm2-libs-pim/Keyboard.def:
* gm2/gm2-libs-pim/Keyboard.mod:
* gm2/gm2-libs-pim/LongIO.def:
* gm2/gm2-libs-pim/LongIO.mod:
* gm2/gm2-libs-pim/Random.def:
* gm2/gm2-libs-pim/Random.mod:
* gm2/gm2-libs-pim/RealConversions.mod:
* gm2/gm2-libs-pim/RealInOut.def:
* gm2/gm2-libs-pim/RealInOut.mod:
* gm2/gm2-libs-pim/Strings.def:
* gm2/gm2-libs-pim/Strings.mod:
* gm2/gm2-libs-pim/Termbase.def:
* gm2/gm2-libs-pim/Termbase.mod:
* gm2/gm2-libs-pim/Terminal.def:
* gm2/gm2-libs-pim/Terminal.mod:
* gm2/gm2-libs-pim/TimeDate.def:
* gm2/gm2-libs-pim/TimeDate.mod:
* gm2/man/Makefile.in:
* gm2/p2c/Makefile.in:
* gm2/p2c/p2c.h:
* gm2/p2c/p2c-src/Makefile.in:
* gm2/p2c/p2c-src/auto-host.h.in:
* gm2/p2c/p2c-src/include/config.h:
* gm2/p2c/p2c-src/include/system.h:
* gm2/p2c/p2c-src/src/Makefile.in:
* gm2/p2c/p2c-src/src/citmods.c:
* gm2/p2c/p2c-src/src/comment.c:
* gm2/p2c/p2c-src/src/decl.c:
* gm2/p2c/p2c-src/src/dir.c:
* gm2/p2c/p2c-src/src/expr.c:
* gm2/p2c/p2c-src/src/funcs.c:
* gm2/p2c/p2c-src/src/hpmods.c:
* gm2/p2c/p2c-src/src/lex.c:
* gm2/p2c/p2c-src/src/loc.p2clib.c:
* gm2/p2c/p2c-src/src/makeproto.c:
* gm2/p2c/p2c-src/src/out.c:
* gm2/p2c/p2c-src/src/p2c-config.h:
* gm2/p2c/p2c-src/src/p2c.h:
* gm2/p2c/p2c-src/src/p2clib.c:
* gm2/p2c/p2c-src/src/parse.c:
* gm2/p2c/p2c-src/src/pexpr.c:
* gm2/p2c/p2c-src/src/stuff.c:
* gm2/p2c/p2c-src/src/trans.c:
* gm2/p2c/p2c-src/src/trans.h:
* gm2/tools-src/def2texi.py:
* gm2/tools-src/mklink.c:
* gm2/ulm-lib-gm2/processes/CoExpressions.def:
* gm2/ulm-lib-gm2/processes/CoExpressions.mod:
* gm2/ulm-lib-gm2/processes/Processes.def:
* gm2/ulm-lib-gm2/processes/Processes.mod:
* gm2/ulm-lib-gm2/std/ASCII.def:
* gm2/ulm-lib-gm2/std/ASCII.mod:
* gm2/ulm-lib-gm2/std/Archive.def:
* gm2/ulm-lib-gm2/std/Archive.mod:
* gm2/ulm-lib-gm2/std/Arguments.def:
* gm2/ulm-lib-gm2/std/Arguments.mod:
* gm2/ulm-lib-gm2/std/Calendar.def:
* gm2/ulm-lib-gm2/std/Calendar.mod:
* gm2/ulm-lib-gm2/std/CallShell.def:
* gm2/ulm-lib-gm2/std/CallShell.mod:
* gm2/ulm-lib-gm2/std/Clock.def:
* gm2/ulm-lib-gm2/std/Clock.mod:
* gm2/ulm-lib-gm2/std/Conversions.def:
* gm2/ulm-lib-gm2/std/Conversions.mod:
* gm2/ulm-lib-gm2/std/Directories.def:
* gm2/ulm-lib-gm2/std/Directories.mod:
* gm2/ulm-lib-gm2/std/Environment.def:
* gm2/ulm-lib-gm2/std/Environment.mod:
* gm2/ulm-lib-gm2/std/EtcGroup.def:
* gm2/ulm-lib-gm2/std/EtcGroup.mod:
* gm2/ulm-lib-gm2/std/Files.def:
* gm2/ulm-lib-gm2/std/Files.mod:
* gm2/ulm-lib-gm2/std/FtdIO.def:
* gm2/ulm-lib-gm2/std/FtdIO.mod:
* gm2/ulm-lib-gm2/std/Functions.def:
* gm2/ulm-lib-gm2/std/Functions.mod:
* gm2/ulm-lib-gm2/std/GetPass.def:
* gm2/ulm-lib-gm2/std/GetPass.mod:
* gm2/ulm-lib-gm2/std/InOut.def:
* gm2/ulm-lib-gm2/std/InOut.mod:
* gm2/ulm-lib-gm2/std/M2EXCEPTION.mod:
* gm2/ulm-lib-gm2/std/M2RTS.mod:
* gm2/ulm-lib-gm2/std/MathLib.def:
* gm2/ulm-lib-gm2/std/MathLib.mod:
* gm2/ulm-lib-gm2/std/Passwd.def:
* gm2/ulm-lib-gm2/std/Passwd.mod:
* gm2/ulm-lib-gm2/std/PipeIO.def:
* gm2/ulm-lib-gm2/std/PipeIO.mod:
* gm2/ulm-lib-gm2/std/Plot.def:
* gm2/ulm-lib-gm2/std/Plot.mod:
* gm2/ulm-lib-gm2/std/RTErrors.def:
* gm2/ulm-lib-gm2/std/RTErrors.mod:
* gm2/ulm-lib-gm2/std/RTExceptions.mod:
* gm2/ulm-lib-gm2/std/RandomGenerator.def:
* gm2/ulm-lib-gm2/std/RandomGenerator.mod:
* gm2/ulm-lib-gm2/std/ReadIntCard.def:
* gm2/ulm-lib-gm2/std/ReadIntCard.mod:
* gm2/ulm-lib-gm2/std/RealConv.def:
* gm2/ulm-lib-gm2/std/RealConv.mod:
* gm2/ulm-lib-gm2/std/RealInOut.def:
* gm2/ulm-lib-gm2/std/RealInOut.mod:
* gm2/ulm-lib-gm2/std/ScanPwfile.def:
* gm2/ulm-lib-gm2/std/ScanPwfile.mod:
* gm2/ulm-lib-gm2/std/StdFuncs.def:
* gm2/ulm-lib-gm2/std/StdFuncs.mod:
* gm2/ulm-lib-gm2/std/StdIO.def:
* gm2/ulm-lib-gm2/std/StdIO.mod:
* gm2/ulm-lib-gm2/std/Storage.def:
* gm2/ulm-lib-gm2/std/Storage.mod:
* gm2/ulm-lib-gm2/std/StrSpec.def:
* gm2/ulm-lib-gm2/std/StrSpec.mod:
* gm2/ulm-lib-gm2/std/StrToNum.def:
* gm2/ulm-lib-gm2/std/StrToNum.mod:
* gm2/ulm-lib-gm2/std/StrToReal.def:
* gm2/ulm-lib-gm2/std/StrToReal.mod:
* gm2/ulm-lib-gm2/std/Strings.def:
* gm2/ulm-lib-gm2/std/Strings.mod:
* gm2/ulm-lib-gm2/std/SysConf.def:
* gm2/ulm-lib-gm2/std/SysConf.mod:
* gm2/ulm-lib-gm2/std/SysPerror.def:
* gm2/ulm-lib-gm2/std/SysPerror.mod:
* gm2/ulm-lib-gm2/std/Terminal.def:
* gm2/ulm-lib-gm2/std/Terminal.mod:
* gm2/ulm-lib-gm2/std/TimeIO.def:
* gm2/ulm-lib-gm2/std/TimeIO.mod:
* gm2/ulm-lib-gm2/sys/Errno.def:
* gm2/ulm-lib-gm2/sys/Errno.mod:
* gm2/ulm-lib-gm2/sys/SYSTEM.def:
* gm2/ulm-lib-gm2/sys/Sys.def:
* gm2/ulm-lib-gm2/sys/Sys.mod:
* gm2/ulm-lib-gm2/sys/SysAccess.def:
* gm2/ulm-lib-gm2/sys/SysAccess.mod:
* gm2/ulm-lib-gm2/sys/SysAlarm.def:
* gm2/ulm-lib-gm2/sys/SysAlarm.mod:
* gm2/ulm-lib-gm2/sys/SysBreak.def:
* gm2/ulm-lib-gm2/sys/SysBreak.mod:
* gm2/ulm-lib-gm2/sys/SysClose.def:
* gm2/ulm-lib-gm2/sys/SysClose.mod:
* gm2/ulm-lib-gm2/sys/SysCreat.def:
* gm2/ulm-lib-gm2/sys/SysCreat.mod:
* gm2/ulm-lib-gm2/sys/SysDup.def:
* gm2/ulm-lib-gm2/sys/SysDup.mod:
* gm2/ulm-lib-gm2/sys/SysExec.def:
* gm2/ulm-lib-gm2/sys/SysExec.mod:
* gm2/ulm-lib-gm2/sys/SysExit.def:
* gm2/ulm-lib-gm2/sys/SysExit.mod:
* gm2/ulm-lib-gm2/sys/SysFcntl.def:
* gm2/ulm-lib-gm2/sys/SysFcntl.mod:
* gm2/ulm-lib-gm2/sys/SysFork.def:
* gm2/ulm-lib-gm2/sys/SysFork.mod:
* gm2/ulm-lib-gm2/sys/SysGetpid.def:
* gm2/ulm-lib-gm2/sys/SysGetpid.mod:
* gm2/ulm-lib-gm2/sys/SysGetuid.def:
* gm2/ulm-lib-gm2/sys/SysGetuid.mod:
* gm2/ulm-lib-gm2/sys/SysIoctl.def:
* gm2/ulm-lib-gm2/sys/SysIoctl.mod:
* gm2/ulm-lib-gm2/sys/SysKill.def:
* gm2/ulm-lib-gm2/sys/SysKill.mod:
* gm2/ulm-lib-gm2/sys/SysLink.def:
* gm2/ulm-lib-gm2/sys/SysLink.mod:
* gm2/ulm-lib-gm2/sys/SysLocations.def:
* gm2/ulm-lib-gm2/sys/SysLocations.mod:
* gm2/ulm-lib-gm2/sys/SysLseek.def:
* gm2/ulm-lib-gm2/sys/SysLseek.mod:
* gm2/ulm-lib-gm2/sys/SysOpen.def:
* gm2/ulm-lib-gm2/sys/SysOpen.mod:
* gm2/ulm-lib-gm2/sys/SysPanic.def:
* gm2/ulm-lib-gm2/sys/SysPanic.mod:
* gm2/ulm-lib-gm2/sys/SysPause.def:
* gm2/ulm-lib-gm2/sys/SysPause.mod:
* gm2/ulm-lib-gm2/sys/SysPipe.def:
* gm2/ulm-lib-gm2/sys/SysPipe.mod:
* gm2/ulm-lib-gm2/sys/SysRead.def:
* gm2/ulm-lib-gm2/sys/SysRead.mod:
* gm2/ulm-lib-gm2/sys/SysSetuid.def:
* gm2/ulm-lib-gm2/sys/SysSetuid.mod:
* gm2/ulm-lib-gm2/sys/SysSignal.def:
* gm2/ulm-lib-gm2/sys/SysSignal.mod:
* gm2/ulm-lib-gm2/sys/SysStat.def:
* gm2/ulm-lib-gm2/sys/SysStat.mod:
* gm2/ulm-lib-gm2/sys/SysTermIO.def:
* gm2/ulm-lib-gm2/sys/SysTermIO.mod:
* gm2/ulm-lib-gm2/sys/SysTime.def:
* gm2/ulm-lib-gm2/sys/SysTime.mod:
* gm2/ulm-lib-gm2/sys/SysUnlink.def:
* gm2/ulm-lib-gm2/sys/SysUnlink.mod:
* gm2/ulm-lib-gm2/sys/SysWait.def:
* gm2/ulm-lib-gm2/sys/SysWait.mod:
* gm2/ulm-lib-gm2/sys/SysWrite.def:
* gm2/ulm-lib-gm2/sys/SysWrite.mod:
* gm2/ulm-lib-gm2/sys/SystemTypes.def:
* gm2/ulm-lib-gm2/sys/SystemTypes.mod:
* gm2/ulm-lib-gm2/sys/UnixString.def:
* gm2/ulm-lib-gm2/sys/UnixString.mod:
* gm2/ulm-lib-gm2/sys/test.mod:
* gm2/www/Makefile.in:
* gm2/www/index.ms:
</pre>
</div>



<div>
<hr />

<a name="rev1.8"></a>


Revision <strong>1.8</strong> -

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?revision=1.8&amp;root=gm2&amp;view=markup">view</a>)

(<a href="/viewvc/*checkout*/gm2/examples/ncurses/ColorText.mod?revision=1.8&amp;root=gm2">download</a>)

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?annotate=1.8&amp;root=gm2">annotate</a>)



- <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.8&amp;view=log">[select for diffs]</a>




<br />

<em>Thu Sep 16 13:54:47 2010 UTC</em> (5 months, 1 week ago) by <em>gaius</em>


<br />Branch:

<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?view=log&amp;root=gm2&amp;pathrev=MAIN"><strong>MAIN</strong></a>







<br />Changes since <strong>1.7: +4 -1 lines</strong>







<br />Diff to <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.7&amp;r2=1.8">previous 1.7</a>










<pre class="vc_log">* Built and released debian package 0.98.
* gm2/version.c:  updated to 0.99 now that the debian
  package 0.98 has been produced.
* gm2/www/index.ms:  updated news of 0.98 release and
  updated information around latest bug fixes.
* gm2/gm2-libs/configure.in:  updated to 0.99.
* gm2/gm2-libs/configure:  rebuilt.
* gm2/examples/ncurses/Makefile:  removed.
* gm2/examples/ncurses/Makefile.in:  added.
* gm2/gm2.texi:  fixed documentation on building hello
  world after building gm2.
</pre>
</div>



<div>
<hr />

<a name="rev1.7"></a>


Revision <strong>1.7</strong> -

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?revision=1.7&amp;root=gm2&amp;view=markup">view</a>)

(<a href="/viewvc/*checkout*/gm2/examples/ncurses/ColorText.mod?revision=1.7&amp;root=gm2">download</a>)

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?annotate=1.7&amp;root=gm2">annotate</a>)



- <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.7&amp;view=log">[select for diffs]</a>




<br />

<em>Thu May 29 18:59:26 2008 UTC</em> (2 years, 9 months ago) by <em>gaius</em>


<br />Branch:

<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?view=log&amp;root=gm2&amp;pathrev=MAIN"><strong>MAIN</strong></a>







<br />Changes since <strong>1.6: +1 -1 lines</strong>







<br />Diff to <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.6&amp;r2=1.7">previous 1.6</a>










<pre class="vc_log">* gm2/Make-lang.in:  tidied up formatting and added
  ability to generate and install shared library
  versions of the pim libraries.
* gm2/gm2-lang.c:  corrected dates.
* gm2/gm2-lang.h:  corrected dates.
* gm2/gm2.texi:  new section index, how to produce
  swig interface files.  Documented -I, -fobject-path=
  and -fmodules flags.  Added new section
  "How to produce swig interface files".
* gm2/gm2spec.c:  corrected dates and added ability
  to force no linking to be done by ld/collect.
  Also added ability to collect all object files.
  Introduced styles of libraries (shared libraries,
  libraries compiled with debugging and optimization
  switches turned on/off).  Front end will now
  generate a -fobject-path= option if one is not
  provided.  Fixed several warnings.
* gm2/gm2spec.c:get_objects.  new function.
* gm2/gm2spec.c:get_style.  new function.
* gm2/gm2spec.c:no_link.  new function.
* gm2/lang-options.h:  -fshared, -fmakeinit, -fobject-path=
  all introduced.
* gm2/lang-specs.h:  modified to enable position independant
  code to be created and linked against.  Also modified to
  reflect change in arguments needed in subsidiary linking
  programs.
* gm2/lang.opt:  list of options includes:  fshared, fmakeinit
  and fobject-path=.
* gm2/examples/executive/Makefile:  modified to utilise
  -fobject-path=.
* gm2/examples/ncurses/ColorText.mod:  fixed bug caught by the
  new stricter compatibility rules.
* gm2/examples/ncurses/WindowDevice.mod:  fixed another bug
  caught by the new stricter compatibility rules.
* gm2/examples/pthread/Makefile:  changed -Wmakeall to -fmakeall.
* gm2/examples/svga/Makefile:  changed -Wmakeall to -fmakeall.
* gm2/examples/swig/full-strlib/Makefile:  removed many rules
  now that automatic linking has been implemented.
* gm2/examples/swig/strlen/Makefile:  changed StrLib to
  MyStrLib to avoid a name clash.
* gm2/examples/swig/strlen/testlen.py:  changed StrLib to
  MyStrLib.
* gm2/examples/swig/strlib/MyStrLib.{def,mod}:  new files.
* gm2/examples/swig/strlib/StrLib.{def,mod}:  removed.
* gm2/gm2-compiler/M2BasicBlock.mod:  fixed dates.
* gm2/gm2-compiler/M2Quads.mod:  implemented
  IsProcedureScope.
* gm2/gm2-compiler/M2Quads.def:  defined
  IsProcedureScope.
* gm2/gm2-compiler/M2Swig.mod:  now explores basic blocks
  of each external procedure to see if parameters are
  in, out or inout.  It also states whether a parameter
  direction is unknown or likely to be in, out or inout.
* gm2/gm2-compiler/gm2lcc.mod:  now understands -shared
  and -fshared and passes all -f options to gcc.
* gm2/gm2-compiler/gm2lgen.mod:  fixed dates and understands
  -fshared.
* gm2/gm2-libs/FIO.{def,mod}:  improved comment.
* gm2/gm2-libs/FpuIO.def:  fixed date and comments.
* gm2/www/index.ms:  updated web page to say documentation
  is built nightly.
</pre>
</div>



<div>
<hr />

<a name="rev1.6"></a>
<a name="gm2_0_51"></a>
<a name="gm2_0_50"></a>
<a name="gm2_0_52"></a>


Revision <strong>1.6</strong> -

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?revision=1.6&amp;root=gm2&amp;view=markup">view</a>)

(<a href="/viewvc/*checkout*/gm2/examples/ncurses/ColorText.mod?revision=1.6&amp;root=gm2">download</a>)

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?annotate=1.6&amp;root=gm2">annotate</a>)



- <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.6&amp;view=log">[select for diffs]</a>




<br />

<em>Wed Jan 11 00:04:43 2006 UTC</em> (5 years, 1 month ago) by <em>gaius</em>


<br />Branch:

<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?view=log&amp;root=gm2&amp;pathrev=MAIN"><strong>MAIN</strong></a>



<br />CVS Tags:

<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?view=log&amp;root=gm2&amp;pathrev=gm2_0_50"><strong>gm2_0_50</strong></a>,

<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?view=log&amp;root=gm2&amp;pathrev=gm2_0_51"><strong>gm2_0_51</strong></a>,

<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?view=log&amp;root=gm2&amp;pathrev=gm2_0_52"><strong>gm2_0_52</strong></a>






<br />Changes since <strong>1.5: +1 -1 lines</strong>







<br />Diff to <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.5&amp;r2=1.6">previous 1.5</a>










<pre class="vc_log">added 2006 to all Copyright dates
</pre>
</div>



<div>
<hr />

<a name="rev1.5"></a>


Revision <strong>1.5</strong> -

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?revision=1.5&amp;root=gm2&amp;view=markup">view</a>)

(<a href="/viewvc/*checkout*/gm2/examples/ncurses/ColorText.mod?revision=1.5&amp;root=gm2">download</a>)

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?annotate=1.5&amp;root=gm2">annotate</a>)



- <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.5&amp;view=log">[select for diffs]</a>




<br />

<em>Fri Nov 18 23:13:36 2005 UTC</em> (5 years, 3 months ago) by <em>gaius</em>


<br />Branch:

<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?view=log&amp;root=gm2&amp;pathrev=MAIN"><strong>MAIN</strong></a>







<br />Changes since <strong>1.4: +1 -1 lines</strong>







<br />Diff to <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.4&amp;r2=1.5">previous 1.4</a>










<pre class="vc_log">fixed LGPL for examples and GPL for examples/map
</pre>
</div>



<div>
<hr />

<a name="rev1.4"></a>


Revision <strong>1.4</strong> -

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?revision=1.4&amp;root=gm2&amp;view=markup">view</a>)

(<a href="/viewvc/*checkout*/gm2/examples/ncurses/ColorText.mod?revision=1.4&amp;root=gm2">download</a>)

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?annotate=1.4&amp;root=gm2">annotate</a>)



- <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.4&amp;view=log">[select for diffs]</a>




<br />

<em>Thu Nov 10 10:31:08 2005 UTC</em> (5 years, 3 months ago) by <em>gaius</em>


<br />Branch:

<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?view=log&amp;root=gm2&amp;pathrev=MAIN"><strong>MAIN</strong></a>







<br />Changes since <strong>1.3: +1 -1 lines</strong>







<br />Diff to <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.3&amp;r2=1.4">previous 1.3</a>










<pre class="vc_log">changed the old FSF address to the new FSF address
</pre>
</div>



<div>
<hr />

<a name="rev1.3"></a>


Revision <strong>1.3</strong> -

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?revision=1.3&amp;root=gm2&amp;view=markup">view</a>)

(<a href="/viewvc/*checkout*/gm2/examples/ncurses/ColorText.mod?revision=1.3&amp;root=gm2">download</a>)

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?annotate=1.3&amp;root=gm2">annotate</a>)



- <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.3&amp;view=log">[select for diffs]</a>




<br />

<em>Thu Nov 10 09:05:47 2005 UTC</em> (5 years, 3 months ago) by <em>gaius</em>


<br />Branch:

<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?view=log&amp;root=gm2&amp;pathrev=MAIN"><strong>MAIN</strong></a>







<br />Changes since <strong>1.2: +1 -1 lines</strong>







<br />Diff to <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.2&amp;r2=1.3">previous 1.2</a>










<pre class="vc_log">* changed all references of the old FSF address to the new address.
</pre>
</div>



<div>
<hr />

<a name="rev1.2"></a>
<a name="gm2_0_49"></a>


Revision <strong>1.2</strong> -

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?revision=1.2&amp;root=gm2&amp;view=markup">view</a>)

(<a href="/viewvc/*checkout*/gm2/examples/ncurses/ColorText.mod?revision=1.2&amp;root=gm2">download</a>)

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?annotate=1.2&amp;root=gm2">annotate</a>)



- <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.2&amp;view=log">[select for diffs]</a>




<br />

<em>Mon Apr 11 10:02:11 2005 UTC</em> (5 years, 10 months ago) by <em>gaius</em>


<br />Branch:

<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?view=log&amp;root=gm2&amp;pathrev=MAIN"><strong>MAIN</strong></a>



<br />CVS Tags:

<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?view=log&amp;root=gm2&amp;pathrev=gm2_0_49"><strong>gm2_0_49</strong></a>






<br />Changes since <strong>1.1: +6 -0 lines</strong>







<br />Diff to <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.1&amp;r2=1.2">previous 1.1</a>










<pre class="vc_log">* fixed examples/executive
</pre>
</div>



<div>
<hr />

<a name="rev1.1"></a>
<a name="gm2_0_44"></a>


Revision <strong>1.1</strong> -

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?revision=1.1&amp;root=gm2&amp;view=markup">view</a>)

(<a href="/viewvc/*checkout*/gm2/examples/ncurses/ColorText.mod?revision=1.1&amp;root=gm2">download</a>)

(<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?annotate=1.1&amp;root=gm2">annotate</a>)



- <a href="/viewvc/gm2/examples/ncurses/ColorText.mod?root=gm2&amp;r1=1.1&amp;view=log">[select for diffs]</a>




<br />

<em>Mon Sep 20 13:41:36 2004 UTC</em> (6 years, 5 months ago) by <em>gaius</em>


<br />Branch:

<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?view=log&amp;root=gm2&amp;pathrev=MAIN"><strong>MAIN</strong></a>



<br />CVS Tags:

<a href="/viewvc/gm2/examples/ncurses/ColorText.mod?view=log&amp;root=gm2&amp;pathrev=gm2_0_44"><strong>gm2_0_44</strong></a>















<pre class="vc_log">* fixed varient record bug as reported by John B. Wallace, Jr
  &lt;<a href="mailto:wallacjb&#64;enter.net">wallacjb&#64;enter.net</a>&gt;. See testsuite/gm2/pim/pass/varient.mod.
* fixed bug in gm2/gm2-compiler/Indexing.mod which
  occurred after the p2c translation.
* in turn this fixed a gm2m problem. The bug resulted in
  touching memory which would not have been malloced so
  this fix may have cured a number of problems.
* fixed the ncurses definition module and WindowDevice module.
* fixed M2RTS:Termination to call termination procedures in
  reverse.
* tidied up gm2-compiler/M2Base.mod and introduced
  compatability matrices which include all the base
  types.
* introduced SHORTINT and SHORTCARD data types.
* added ncurses.def example in examples/ncurses
</pre>
</div>

 



 <hr />
<p><a name="diff"></a>
This form allows you to request diffs between any two revisions of this file.
For each of the two "sides" of the diff,

select a symbolic revision name using the selection box, or choose
'Use Text Field' and enter a numeric revision.

</p>
<form method="get" action="/viewvc/gm2/examples/ncurses/ColorText.mod" id="diff_select">
<table cellpadding="2" cellspacing="0" class="auto">
<tr>
<td>&nbsp;</td>
<td>
<input type="hidden" name="root" value="gm2" /><input type="hidden" name="view" value="diff" />
Diffs between

<select name="r1">
<option value="text" selected="selected">Use Text Field</option>

<option value="1.6:gm2_0_52">gm2_0_52</option>

<option value="1.6:gm2_0_51">gm2_0_51</option>

<option value="1.6:gm2_0_50">gm2_0_50</option>

<option value="1.2:gm2_0_49">gm2_0_49</option>

<option value="1.1:gm2_0_44">gm2_0_44</option>

<option value="1.11:MAIN">MAIN</option>

<option value="1.11:HEAD">HEAD</option>

</select>
<input type="text" size="12" name="tr1"
value="1.11"
onchange="document.getElementById('diff_select').r1.selectedIndex=0" />

and

<select name="r2">
<option value="text" selected="selected">Use Text Field</option>

<option value="1.6:gm2_0_52">gm2_0_52</option>

<option value="1.6:gm2_0_51">gm2_0_51</option>

<option value="1.6:gm2_0_50">gm2_0_50</option>

<option value="1.2:gm2_0_49">gm2_0_49</option>

<option value="1.1:gm2_0_44">gm2_0_44</option>

<option value="1.11:MAIN">MAIN</option>

<option value="1.11:HEAD">HEAD</option>

</select>
<input type="text" size="12" name="tr2"
value="1.1"
onchange="document.getElementById('diff_select').r2.selectedIndex=0" />

</td>
</tr>
<tr>
<td>&nbsp;</td>
<td>
Type of Diff should be a
<select name="diff_format" onchange="submit()">
<option value="h" selected="selected">Colored Diff</option>
<option value="l" >Long Colored Diff</option>
<option value="u" >Unidiff</option>
<option value="c" >Context Diff</option>
<option value="s" >Side by Side</option>
</select>
<input type="submit" value=" Get Diffs " />
</td>
</tr>
</table>
</form>


<form method="get" action="/viewvc/gm2/examples/ncurses/ColorText.mod">
<div>
<hr />
<a name="logsort"></a>
<input type="hidden" name="root" value="gm2" /><input type="hidden" name="view" value="log" />
Sort log by:
<select name="logsort" onchange="submit()">
<option value="cvs" >Not sorted</option>
<option value="date" selected="selected">Commit date</option>
<option value="rev" >Revision</option>
</select>
<input type="submit" value=" Sort " />
</div>
</form>


<hr />
<table>
<tr>
<td><address>Send suggestions and report system problems to the <a href="https://savannah.gnu.org/support/?group=administration">Savannah Hackers</a>.</address></td>
<td style="text-align: right;"><strong><a href="/viewcvs-doc/help_log.html">ViewVC Help</a></strong></td>
</tr>
<tr>
<td>Powered by <a href="http://viewvc.tigris.org/">ViewVC 1.0.7</a></td>
<td style="text-align: right;">&nbsp;</td>
</tr>
</table>
</body>
</html>


