// physical usb part

module usbphys(DP, DN, dif, send, dp, dn);
   inout DP, DN;
   output dif;
   input  send, dp, dn;

   assign dif = DP & ~DN;
   assign { DP, DN } = send ? { dp, dn } : 2'bzz;

endmodule // usbphys
