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

import       RVBS :: *;
import BlueBasics :: *;
import        BID :: *;

//////////////////////
// Core RVBS module //
////////////////////////////////////////////////////////////////////////////////

module [Module] mkRVBSCore#(RVState s,
                   function ISADefModule#(Empty) init (RVState st),
                   function ISADefModule#(Empty) iFetch (RVState st))
  (BIDProbes);

  // instanciating simulator
  let modList = list(init, iFetch, mkRVCommon, mkRVTrap, mkBase_RV32I);
  `ifdef RVM
    modList = append(modList, list(mkExt_32_M));
  `endif
  `ifdef RVC
    modList = append(modList, list(mkExt_32_C));
  `endif
  `ifdef XLEN64
  modList = append(modList, list(mkBase_RV64I));
    `ifdef RVM
      modList = append(modList, list(mkExt_64_M));
    `endif
    `ifdef RVC
      modList = append(modList, list(mkExt_64_C));
    `endif
  `endif
  `ifdef RVZICSR
    modList = append(modList, list(mkExt_Zicsr));
  `endif
  `ifdef RVZIFENCEI
    modList = append(modList, list(mkExt_Zifencei));
  `endif
  `ifdef RVXCHERI
    modList = append(modList, list(mkExt_Xcheri));
  `endif
  let bid_probes <- mkISASim(s, modList);

  return bid_probes;

endmodule
