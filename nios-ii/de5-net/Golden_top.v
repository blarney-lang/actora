// Copyright (c) 2016-2017 Alex Forencich
// Copyright (c) 2017 Matthew Naylor
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

module Golden_top(
  input wire OSC_50_B3B,
  input wire OSC_50_B3D,
  input wire OSC_50_B4A,
  input wire OSC_50_B4D,
  input wire OSC_50_B7A,
  input wire OSC_50_B7D,
  input wire OSC_50_B8A,
  input wire OSC_50_B8D,
  input wire [3:0] BUTTON,
  input wire CPU_RESET_n
);

wire clk_50mhz = OSC_50_B7A;
wire rst_50mhz_n = CPU_RESET_n;

S5_DDR3_QSYS u0 (
  .clk_clk       (clk_50mhz),
  .reset_reset_n (rst_50mhz_n)
);

endmodule 
