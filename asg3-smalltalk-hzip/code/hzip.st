#!/usr/bin/gst -f
"
Program utilizes huffman encoding to compress and decompress files
Pseudocode outline: 
Input file is required.
Exactly one of four input arguments are also required: 

  -d: print debug information

  -t: Compression algorithm is activated, and decoding tree is printed
      to the standard output. Output filename may not be specified

  -c: Input file is compressed and written to the output file, if
      specified, or to the standard output, if not.

  -u: The input file is assumed to be compressed, and is uncompressed,
      written to the output file, if specified, or to stdout if not.
"
