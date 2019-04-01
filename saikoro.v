// this can run by https://www.jdoodle.com/execute-verilog-online

module saikoro(ck, reset, enable, lamp);
  input        ck, reset, enable;
  output [6:0] lamp;
  reg    [2:0] cnt;
  
  always @( posedge ck ) begin
    if (reset)
      cnt <= 1;
    else if (enable)
      if (cnt == 6)
        cnt <= 1;
      else
        cnt <= cnt + 1;
  end
  
  function [6:0] dec;
    input [2:0]   in;
    case (in)
      1: dec = 7'b0001000;
      2: dec = 7'b1000001;
      3: dec = 7'b0011100;
      4: dec = 7'b1010101;
      5: dec = 7'b1011101;
      6: dec = 7'b1110111;
    endcase
  endfunction
  
  assign lamp = dec(cnt);
  
endmodule

module saikoro_test;
  reg   ck, reset, enable;
  wire  [6:0] lamp;
  
  parameter STEP=1000;
  
  always #(STEP/2) ck = ~ck;
  
  saikoro sai( ck, reset, enable, lamp );
  
  initial begin
    ck = 0;
    #STEP reset = 1; enable = 0;
    #STEP reset = 0;
    #STEP enable = 1;
    #(STEP*5)
          enable = 0;
    #STEP enable = 1;
    #(STEP*5)
        $finish;
  end
  
  initial $monitor( $stime, " en=%b sai=%h lamp=%b" , 
                   enable, sai.cnt, lamp
                   );
    
               
endmodule // saikoro_test
