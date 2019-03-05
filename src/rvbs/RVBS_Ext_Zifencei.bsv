/*-
 * Copyright (c) 2019 Alexandre Joannou
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

import BID        :: *;
import BitPat     :: *;
import BlueUtils  :: *;
import RVBS_Types :: *;

// funct3 = FENCE.I = 001
// opcode = 0001111
function Action instrFENCE_I(RVState s) = action
  //TODO
  printTLogPlusArgs("itrace", $format("pc: 0x%0x -- fence.i", s.pc));
endaction;

module [ISADefModule] mkExt_Zifencei#(RVState s) ();
  defineInstEntry("fence.i", pat(n(4'b0000), n(4'b0000), n(4'b0000), n(5'b00000), n(3'b001), n(5'b00000), n(7'b0001111)), instrFENCE_I(s));
endmodule
