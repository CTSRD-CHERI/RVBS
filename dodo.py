#-
# Copyright (c) 2018 Alexandre Joannou
# All rights reserved.
#
# This software was developed by SRI International and the University of
# Cambridge Computer Laboratory (Department of Computer Science and
# Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
# DARPA SSITH research programme.
#
# @BERI_LICENSE_HEADER_START@
#
# Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
# license agreements.  See the NOTICE file distributed with this work for
# additional information regarding copyright ownership.  BERI licenses this
# file to you under the BERI Hardware-Software License, Version 1.0 (the
# "License"); you may not use this file except in compliance with the
# License.  You may obtain a copy of the License at:
#
#   http://www.beri-open-systems.org/legal/license-1-0.txt
#
# Unless required by applicable law or agreed to in writing, Work distributed
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations under the License.
#
# @BERI_LICENSE_HEADER_END@
#

import re
import os, errno
import os.path as op
import tempfile as temp
import subprocess as sub

from doit.task import clean_targets
from doit.action import CmdAction

# RVBS class to hold a project's config
class RVBS:
  def __init__(self,
    size = 32, mem_width = None,
    mem_size = 16384, mem_img="test-prog.hex",
    s_mode = False, u_mode = False,
    c_ext = False, n_ext = False,
    pmp = False):
    self.size = size
    if mem_width:
      self.mem_width = mem_width
    else:
      self.mem_width = self.size
    self.mem_size = mem_size
    self.mem_img = mem_img
    self.s_mode = s_mode
    self.u_mode = u_mode
    self.c_ext = c_ext
    self.n_ext = n_ext
    self.pmp = pmp

  def name(self):
    name = "rv{:d}i{:s}{:s}".format(
      self.size,
      "c" if self.c_ext else "",
      "n" if self.n_ext else "")
    if (self.s_mode or self.u_mode):
      name += "-{:s}{:s}".format(
        "s" if self.s_mode else "",
        "u" if self.u_mode else "")
    if self.pmp:
      name += "-pmp"
    return name

  def bsc_flags(self):
    flags = ["-D", "PRINT_ABI_REG_NAME"]
    flags += ["-D", "XLEN32"]
    if self.size >= 64:
      flags += ["-D","XLEN64"]
    flags += ["-D", "MEM_SIZE={:d}".format(self.mem_size)]
    flags += ["-D", "MEM_IMG=\"{:s}\"".format(self.mem_img)]
    if self.s_mode:
      flags += ["-D","SUPERVISOR_MODE"]
    if self.u_mode:
      flags += ["-D","USER_MODE"]
    if self.c_ext:
      flags += ["-D","RVC"]
    if self.n_ext:
      flags += ["-D","RVN"]
    if self.pmp:
      flags+=["-D","PMP"]
    return flags

  def rv_tests(self):
    if self.size == 32:
      tests = ["rv32i"]
      if self.c_ext:
        tests += ["rv32c"]
    elif self.size == 64:
      tests = ["rv64i"]
      if self.c_ext:
        tests += ["rv64c"]
    return tests
    #tests = ["rv32i"]
    #if self.c_ext:
    #  tests += ["rv32c"]
    #if self.size == 64:
    #  tests += ["rv64i"]
    #  if self.c_ext:
    #    tests += ["rv64c"]
    #return tests

# list of all project's configs
rvbss = [
  RVBS( size = sz,
    s_mode = s, u_mode = u,
    c_ext = c, n_ext = n,
    pmp = pmp)
  for sz in [32, 64]
  for s in [False, True] for u in [False, True]
  for c in [False, True] for n in [False, True]
  for pmp in [False, True]
]

# python flatten list of list to list
def flatten(l):
  return [x for ls in l for x in ls]

# root dir
root_dir = os.getcwd()
# markdown docs
pandoc = sub.run(["which","pandoc"],stdout=sub.PIPE).stdout.decode("utf-8").strip()
md_docs = [x for x in os.listdir(root_dir) if x[-3:] == ".md"]
# test paths
tests_dir = op.join(root_dir,"rv-tests")
test32i_re = re.compile("rv32(mi|ui)-(uo|rvbs)-[^.]+$")
test32c_re = re.compile("rv32uc-(uo|rvbs)-[^.]+$")
test64i_re = re.compile("rv64(mi|ui)-(uo|rvbs)-[^.]+$")
test64c_re = re.compile("rv64uc-(uo|rvbs)-[^.]+$")
tests = {
  'rv32i': [f for f in os.listdir(tests_dir) if re.match(test32i_re,f)],
  'rv32c': [f for f in os.listdir(tests_dir) if re.match(test32c_re,f)],
  'rv64i': [f for f in os.listdir(tests_dir) if re.match(test64i_re,f)],
  'rv64c': [f for f in os.listdir(tests_dir) if re.match(test64c_re,f)]
}
# test pass
test_pass_re = re.compile("TEST SUCCESS")

# tools
objcopy = sub.run(["which","riscv64-unknown-elf-objcopy"],stdout=sub.PIPE).stdout.decode("utf-8").strip()
ihex2img = op.expanduser("~/ihex-to-img.py")
# bluespec
#forkjoindir=op.expanduser("~/devstuff/ForkJoin")
recipedir=op.expanduser("~/devstuff/Recipe")
bitpatdir=op.expanduser("~/devstuff/BitPat")
biddir=op.expanduser("~/devstuff/BID")
#bsvpath=":".join(["+",forkjoindir,bitpatdir,biddir])
bsvpath=":".join(["+",recipedir,bitpatdir,biddir])
bsc_flags=["-p",bsvpath,"-check-assert"]
bsc_flags+=["-show-schedule"]
#bsc_flags+=["-show-rule-rel", "*", "*"]
bsc = sub.run(["which","bsc"],stdout=sub.PIPE).stdout.decode("utf-8").strip()
topmod = "rvbs"
topfile = "Top.bsv"
cfiles = [op.join(biddir,"BID_SimUtils.c"),op.join(biddir,"BID_Utils_SimMem.c")]
#gcc
cc="gcc-4.8"
cxx="g++-4.8"
# bluesim simulator
def fullname (s, hasI, hasC,hasPMP):
    return "rv{:d}{:s}{:s}{:s}".format(s, "i" if hasI else "", "c" if hasC else "", "-pmp" if hasPMP else "")
def simname (s, hasC, hasPMP):
    return fullname(s, True, hasC, hasPMP)
def bdir (rvbs):
  return "{:s}-bdir".format(rvbs.name())
def simdir (rvbs):
  return "{:s}-simdir".format(rvbs.name())
proglink = "test-prog.hex"
tracedir = op.join(root_dir,"test-traces")
debug = True
totalmemsz = 16384

# from https://stackoverflow.com/questions/10840533/most-pythonic-way-to-delete-a-file-which-may-not-exist
def silentremove(filename):
  try:
    os.remove(filename)
  except OSError as e:
    if e.errno != errno.ENOENT:
      raise

def in_root_dir(fname):
  return op.join(root_dir, fname)

##########################
# make bluesim simulator #
################################################################################
def task_build_simulator () :
  """Builds the Bluesim simulator"""

  def build_sim(rvbs):
    silentremove(in_root_dir(rvbs.name()))
    silentremove(in_root_dir(rvbs.name()+".so"))
    os.makedirs(bdir(rvbs),exist_ok=True)
    os.makedirs(simdir(rvbs),exist_ok=True)
    more_bsc_flags = ["-bdir",bdir(rvbs),"-simdir",simdir(rvbs)]
    cmd =  [bsc] + bsc_flags + more_bsc_flags + rvbs.bsc_flags() + ["-sim","-g",topmod,"-u",topfile]
    sub.run(cmd)
    env2 = os.environ.copy()
    env2["CC"] = cc
    env2["CXX"] = cxx
    cmd = [bsc] + bsc_flags + more_bsc_flags + rvbs.bsc_flags() + ["-sim","-o",in_root_dir(rvbs.name()),"-e",topmod] + cfiles
    sub.run(cmd,env=env2)

  for rvbs in rvbss:
    yield {
      'name'    : rvbs.name(),
      'actions' : [(build_sim,[rvbs])],
      #'file_dep': [],
      'clean'   : [CmdAction("rm -rf {:s} {:s} {:s} {:s}.so".format(bdir(rvbs), simdir(rvbs), rvbs.name(), rvbs.name()))],
      'targets' : [in_root_dir(rvbs.name()), in_root_dir(rvbs.name()+".so")],
      'verbosity':2
    }


############################
# Convert test elfs to hex #
################################################################################

def test_name(f, m_sz):
  return "{:s}-mem{:d}".format(f,m_sz)

def task_test_elf_to_ihex () :
  """Converts an elf test to an ihex"""

  def elf_to_ihex (f):
    print("gen ihex for {:s}".format(f))
    cmd = [objcopy,"-O","ihex"]
    cmd += ["--only-section=.text.init","--only-section=.text","--only-section=.data"]
    #cmd += ["--only-section=.text","--only-section=.data"]
    cmd += [f,f+".ihex"]
    sub.run(cmd)

  for t in [x for sublist in tests.values() for x in sublist]:
    yield {
      'name'    : t,
      'actions' : [(elf_to_ihex,[op.join(tests_dir,t)])],
      'file_dep': [op.join(tests_dir,t)],
      'clean'   : [clean_targets],
      'targets' : [op.join(tests_dir, t+".ihex")],
      'verbosity':2
    }

def task_test_ihex_to_hex () :
  """Converts an ihex test to a hex"""

  def ihex_to_hex (f, m):
    print("gen hex for {:s} (mem{:d})".format(f,m))
    outhex = open(test_name(f, m)+".hex","w")
    cmd = [ihex2img,f+".ihex"]
    cmd += ["hex","0",str(int(m/8)),str(totalmemsz)]
    sub.run(cmd,stdout=outhex)

  #for t, m in [(x, z) for z in [32, 64] for y in [32,64] for x in tests[fullname(y,True,False,False)]+tests[fullname(y,False,True,False)]]:
  for t, m in [(x, y) for y in [32,64] for x in tests[fullname(y,True,False,False)]+tests[fullname(y,False,True,False)]]:
    yield {
      'name'    : test_name(t, m),
      'actions' : [(ihex_to_hex,[op.join(tests_dir,t), m])],
      'file_dep': [op.join(tests_dir,t+".ihex")],
      'clean'   : [clean_targets],
      'targets' : [op.join(tests_dir, test_name(t, m)+".hex")],
      'verbosity':2
    }

#############
# Run tests #
################################################################################
def task_run_test () :
  """Runs a test on the simulator"""

  def run_test (rvbs, test, timeout_sec=2):
    os.makedirs(tracedir,exist_ok=True)
    with temp.TemporaryDirectory() as tmpd:
      os.chdir(tmpd)
      #os.symlink(op.join(root_dir,rvbs.name()),rvbs.name())
      os.symlink(op.join(root_dir,rvbs.name())+".so",rvbs.name()+".so")
      os.symlink(op.join(tests_dir, test+".hex"),proglink)
      bluespecdir = os.environ.get('BLUESPECDIR')
      bluesim = op.join(bluespecdir,"tcllib/bluespec/bluesim.tcl")
      #cmd = [bluesim, "{:s}.so".format(rvbs.name()), "rvbs", "--script_name", rvbs.name()]
      cmd = [bluesim, "{:s}.so".format(rvbs.name()), "rvbs"]
      if debug:
          #cmd += ["+itrace","+CSRs","+BID_Core","+BID_Utils"]
          #cmd += ["+itrace","+CSRs","+BID_Utils"]
          cmd += ["+itrace", "+CSRs"]
      trace = op.join(tracedir, "-".join([rvbs.name(),test]))
      silentremove(trace)
      with open(trace,"w") as trace_file:
        try:
          sub.run(cmd,stdout=trace_file,timeout=timeout_sec)
        except sub.TimeoutExpired:
            print("\x1b[31m{:s} - {:s} timed out after {:d} seconds\x1b[0m".format(rvbs.name(), test, timeout_sec))
            trace_file.seek(0,2) # get to the end of the file
            trace_file.write("\nTIMEOUT\n")

  for rvbs in rvbss:
    for t in flatten(tests[x] for x in rvbs.rv_tests()):
      tname = test_name(t, rvbs.mem_width)
      yield {
        'name'    : "-".join([rvbs.name(),tname]),
        'actions' : [(run_test,[rvbs, tname])],
        'file_dep': [op.join(tests_dir, tname+".hex"), in_root_dir(rvbs.name()+".so")],
        'clean'   : [clean_targets],
        'targets' : [op.join(tracedir, "-".join([rvbs.name(), tname]))],
        'verbosity':2
      }

def task_check_tests ():
  "verify a test's status"

  def log(f, msg):
    f.write(msg+"\n")
    print(msg)

  def check_tests(rvbs, tests):
    name = rvbs.name()
    rpt_file = op.join(tracedir, "tests-report-{:s}.txt".format(name))
    silentremove(rpt_file)
    with open(rpt_file, "w") as rpt:
      successes = 0
      failures = 0
      log(rpt, "\n--------------------\n-- {:s} tests: \n--------------------\n".format(name))
      for test in tests:
        tracefile = open(test,"r")
        matches = [l for l in tracefile if re.match(test_pass_re,l)]
        success = len(matches) != 0
        if success:
          successes += 1
          log(rpt, '\x1b[32m'+test+" success"'\x1b[0m')
        else:
          failures += 1
          log(rpt, '\x1b[31m'+test+" fail"+'\x1b[0m')
      successes_str = "\x1b[32m{:d} success(es)\x1b[0m".format(successes) if (successes > 0) else None
      failures_str  = "\x1b[31m{:d} failure(s)\x1b[0m".format(failures) if (failures > 0) else None
      if successes_str and failures_str:
        log(rpt, "{:s} tests: {:s} / {:s}".format(name, successes_str, failures_str))
      elif successes_str:
        log(rpt, "{:s} tests: {:s}".format(name, successes_str))
      else:
        log(rpt, "{:s} tests: {:s}".format(name, failures_str))

  for rvbs in rvbss:
    runtests = flatten(tests[x] for x in rvbs.rv_tests())
    test_traces = [op.join(tracedir,"-".join([rvbs.name(), test_name(t, rvbs.mem_width)])) for t in runtests]
    yield {
      'name'    : rvbs.name(),
      'actions' : [(check_tests,[rvbs, test_traces])],
      'file_dep': test_traces,
      'targets' : ["tests-report-{:s}.txt".format(rvbs.name())],
      'clean'   : [clean_targets],
      'uptodate': [False],
      'verbosity':2
    }

def task_render_markdown ():
  "renders markdown documents"

  def render_doc(inpt, outpt):
    cmd = [pandoc,"-t","html","-o",outpt,inpt]
    sub.run(cmd)

  for doc in md_docs:
    noext = op.splitext(doc)[0]
    yield {
      'name'    : noext,
      'actions' : [(render_doc,[noext+".md", noext+".html"])],
      'file_dep': [noext+".md"],
      'targets' : [noext+".html"],
      'clean'   : [clean_targets],
      'verbosity':2
    }
