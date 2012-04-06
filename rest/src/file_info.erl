-module(file_info).
-compile(export_all).


%% Gif header, width and then height
file_info(<<71,73,70,56,55,97,_/binary>>) -> "image/gif";
file_info(<<71,73,70,56,57,97,_/binary>>) -> "image/gif";

%% PNG header, chunk1, width and height
%% spec: http://www.w3.org/TR/PNG/#
file_info(<<137,80,78,71,13,10,26,10,_/binary>>) -> "image/png";

%% JPEG
%% ref: http://www.obrador.com/essentialjpeg/headerinfo.htm
%% thanks http://www.ravenna.com/gifs/isize.pl
file_info(<<255,216,255,_/binary>>) -> "image/jpeg";
file_info(<<82,97,114,_/binary>>) ->  "application/x-rar";
file_info(<<70,76,86,_/binary>>) ->  "application/octet-stream";
file_info(<<66,77,_/binary>>) ->  "image/bmp";
file_info(Data) when byte_size(Data) > 100 ->  
    Length = byte_size(Data),
	case binary_to_list(Data,Length-127,Length-125) of
	      "TAG" -> "audio/mpeg";
		  _ -> "text/css"
	end;
file_info(_)  -> "text/css".