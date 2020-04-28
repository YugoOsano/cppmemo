//-- bitmap read/write and manipulation using a library from:
//   https://www.partow.net/programming/bitmap/index.html
//
// commands are:
// wget https://www.partow.net/downloads/bitmap.zip
// unzip bitmap.zip
// cd bitmap/
// g++ bitmap_test.cpp
// ./a.out
// (-> several bmp files will be created)
// eog [bmp file]

// class/method list is at:
// https://www.partow.net/programming/bitmap/documentation/index.html

// about bitmap format, see
// http://www.umekkii.jp/data/computer/file_format/bitmap.cgi

// the following example is transcribed from Example01 on the above.
// One of files exported by bitmap_test can be used as the input image. 
#include <cstdio>
#include "bitmap/bitmap_image.hpp"

int main()
{
  // the content of an image instance is like:
  //--  {file_name_ = "bitmap/input.bmp",
  //     width_ = 1000,
  //     height_ = 1000,
  //     row_increment_ = 3000,
  //     bytes_per_pixel_ = 3,
  //     channel_mode_ = bitmap_image::bgr_mode,
  //     data_ = std::vector of length 3000000,
  //                            capacity 3000000
  //     = {102 'f', 0 '\000', 29 '\035', 107 'k', 0 '\000', 23 '\027', 112 'p', 0..... 
   bitmap_image image("bitmap/input.bmp");

   if (!image)
   {
      printf("Error - Failed to open: input.bmp\n");
      return 1;
   }

   unsigned int total_number_of_pixels = 0;

   const unsigned int height = image.height();
   const unsigned int width  = image.width();

   for (std::size_t y = 0; y < height; ++y)
   {
      for (std::size_t x = 0; x < width; ++x)
      {
	// struct rgb_t {
	//   unsigned char red;
	//   unsigned char green;
	//   unsigned char blue;
	// };
         rgb_t colour;

         image.get_pixel(x, y, colour);

	 if (x%10 == 0 and y%10==0)
	   printf("(x: %ld, y: %ld), (R %d, G %d, B %d)\n",
		  x,y,
		  colour.red,
		  colour.green,
		  colour.blue); 
	 
         if (colour.red >= 111)
            total_number_of_pixels++;
      }
   }

   printf("Number of pixels with red >= 111: %d\n",total_number_of_pixels);

   return 0;
}
