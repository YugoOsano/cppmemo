// https://www.hiroom2.com/2014/05/28/libpng%E3%81%AEc-%E3%82%A4%E3%83%B3%E3%82%BF%E3%83%BC%E3%83%95%E3%82%A7%E3%83%BC%E3%82%B9%E3%81%A7%E3%81%82%E3%82%8Bpng-%E3%82%92%E4%BD%BF%E3%81%A3%E3%81%A6%E3%81%BF%E3%82%8B/

/* to install to WSL:
   sudo apt install libpng++-dev

   to see relevant library:
   dpkg -L libpng++-dev

   to see linking option:
   libpng-config --libs

   then compile command is (-lpng12 must be after cc file):
   g++ libpng_practice.cc -lpng12

   eog is a light weight image browser
 */

#include <png++/png.hpp>
#include <stdio.h>

//-- example transcribed from:
// http://www.labbookpages.co.uk/software/imgProc/libPNG.html
int writeImage(char*  filename,
	       int    width,
	       int    height,
	       float* buffer,
	       char*  title)
{
   int code = 0;
   FILE *fp = NULL;
   png_structp png_ptr = NULL;
   png_infop info_ptr = NULL;
   png_bytep row = NULL;
   // Open file for writing (binary mode)
   fp = fopen(filename, "wb");
   if (fp == NULL) {
     fprintf(stderr, "Could not open file %s for writing\n", filename);
     code = 1;
     goto finalise;
   }
   // Initialize write structure
   png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
   if (png_ptr == NULL) {
      fprintf(stderr, "Could not allocate write struct\n");
      code = 1;
      goto finalise;
   }

   // Initialize info structure
   info_ptr = png_create_info_struct(png_ptr);
   if (info_ptr == NULL) {
      fprintf(stderr, "Could not allocate info struct\n");
      code = 1;
      goto finalise;
   }
   // Setup Exception handling
   if (setjmp(png_jmpbuf(png_ptr))) {
     fprintf(stderr, "Error during png creation\n");
     code = 1;
     goto finalise;
   }
    png_init_io(png_ptr, fp);

   // Write header (8 bit colour depth)
   png_set_IHDR(png_ptr, info_ptr, width, height,
         8, PNG_COLOR_TYPE_RGB, PNG_INTERLACE_NONE,
         PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);

   // Set title
   if (title != NULL) {
     png_text title_text;
     title_text.compression = PNG_TEXT_COMPRESSION_NONE;
     title_text.key = "Title";
     title_text.text = title;
     png_set_text(png_ptr, info_ptr, &title_text, 1);
   }
   png_write_info(png_ptr, info_ptr);
   // Allocate memory for one row (3 bytes per pixel - RGB)
   row = (png_bytep) malloc(3 * width * sizeof(png_byte));

   // Write image data
   int x, y;
   for (y=0 ; y<height ; y++) {
     for (x=0 ; x<width ; x++) {
       //setRGB(&(row[x*3]), buffer[y*width + x]);
     }
     png_write_row(png_ptr, row);
   }

   // End write
   png_write_end(png_ptr, NULL);
 finalise:
   if (fp != NULL) fclose(fp);
   if (info_ptr != NULL) png_free_data(png_ptr, info_ptr, PNG_FREE_ALL, -1);
   if (png_ptr != NULL) png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
   if (row != NULL) free(row);
   return code;
}

int main() {
  //png::image<png::rgb_pixel> image("input.png");

  float *buffer;
  writeImage("aaa.png",//char*  filename,
	     50,//   width,
	     50,//   height,
	     buffer,
	     "aaa");//char*  title
  return 0;
}
