/*-
 * Copyright (c) 2018 Alexandre Joannou
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

import FIFOF :: *;

import BID :: *;
import BlueBasics :: *;
import BitPat :: *;
import Recipe :: *;
import RVBS_StateTypes :: *;
import RVBS_MemAccess :: *;

module [ISADefModule] mkRVCommon#(RVState s) (Empty);

  // Memory commons
  `ifdef RVXCHERI
  match {.rHandle, .rDest, .rNumBytes, .rSgnExt, .rCap} = s.readMem.first;
  match {.wHandle, .wNumBytes, .wData, .wCap} = s.writeMem.first;
  `else
  match {.rVaddr, .rDest, .rNumBytes, .rSgnExt} = s.readMem.first;
  match {.wVaddr, .wNumBytes, .wData} = s.writeMem.first;
  `endif
  // handle mem requests
  defineEpiEntry(rOneMatch(list(s.readMem.notEmpty, s.writeMem.notEmpty),
                           list(
                             // handle reads
                             rFastSeq(rBlock(
                               `ifdef RVXCHERI
                               doReadMem(s, rHandle, rDest, rNumBytes, rSgnExt, rCap),
                               `else
                               doReadMem(s, rVaddr, rDest, rNumBytes, rSgnExt),
                               `endif
                               s.readMem.deq
                             )),
                             // handle writes
                             rFastSeq(rBlock(
                               `ifdef RVXCHERI
                               doWriteMem(s, wHandle, wNumBytes, wData, wCap),
                               `else
                               doWriteMem(s, wVaddr, wNumBytes, wData),
                               `endif
                               s.writeMem.deq
                             ))
                           ),
                           rAct(noAction)));

endmodule
