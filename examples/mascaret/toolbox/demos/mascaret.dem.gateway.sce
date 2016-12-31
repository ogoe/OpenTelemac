//
// This file is released under the 3-clause BSD license. See COPYING-BSD.

function subdemolist = demo_gateway()
  demopath = get_absolute_file_path("mascaret.dem.gateway.sce");

  subdemolist = ["Normal flow conditions" ,"Uniform.sce"
                 "Schematic flood wave" ,"Flood.sce"
                 "Dam break wave" ,"Ritter.sce"
				 ];

  subdemolist(:,2) = demopath + subdemolist(:,2);
  
endfunction

subdemolist = demo_gateway();
clear demo_gateway; // remove demo_gateway on stack

