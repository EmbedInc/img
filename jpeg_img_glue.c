/*   Module of "glue" routines between the Cognivision IMG library code
**   and the IJG JPEG library code.  This is a C source file, so it includes
**   the IJG header file JPEGLIB.H.  Features are exported to the
**   Cognivision code in a manner independent of the IJG data structure
**   details.  The interface presented to the Cognivision code is defined
**   in JPEG_IMG.INS.PAS.
*/
#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <math.h>
#include "jpeglib.h"

/*******************************************************************
**
**   The following declarations must be manually kept in sync with those
**   in JPEG_IMG.INS.PAS.
*/
#define jpeg_pixform_rgb24_k 0    /* 24 bit RGB pixel format */
#define jpeg_pixform_gray8_k 1    /* 8 bit gray scale pixel format */

typedef int sys_int_adr_t;        /* integer that holds a memory address */
typedef int sys_int_machine_t;    /* standard machine integer */
/*
**   This structure must match the start of the internal structure used
**   to manage one JPEG stream writing instance.
*/
typedef struct app_write_t {
  struct jpeg_compress_struct * cinfo_p; /* pointer to JPEG library out stream info */
  void * buf_p;                   /* pointer to start of output buffer */
  sys_int_adr_t bufsize;          /* output buffer size in bytes */
  sys_int_adr_t bufn;             /* number of bytes actually in the buffer */
  void (* writebuf) (             /* routine to write contents of output buffer */
    struct app_write_t *);
  } app_write_t;
/*
**   This structure must match the start of the internal structure used
**   to manage one JPEG stream reading instance.
*/
typedef struct app_read_t {
  struct jpeg_decompress_struct * cinfo_p; /* pointer to JPEG lib in stream info */
  void * buf_p;                   /* pointer to start of input buffer */
  sys_int_adr_t bufsize;          /* input buffer size in bytes */
  sys_int_adr_t bufn;             /* number of unread bytes left in input buffer */
  sys_int_machine_t pixx;         /* image width in pixels */
  sys_int_machine_t pixy;         /* image height in pixels */
  sys_int_machine_t ncomp;        /* number of component values per pixel */
  unsigned char pixform;          /* pixel format ID */
  void (* readbuf) (              /* routine to read more data into input buffer */
    struct app_read_t *);
  } app_read_t;

/*******************************************************************
**
**   Local subroutine OUTPUT_INIT (CINFO_P)
**
**   This routine is called by the JPEG library to initialize output
**   data writing.
*/
static void output_init (
  struct jpeg_compress_struct * cinfo_p) {

app_write_t * app_p;              /* pointer to application data */

/*
**   Start of executable code.
*/
  app_p = cinfo_p->client_data;   /* get pointer to application private data */

  cinfo_p->dest->next_output_byte = /* set next byte pointer to buffer start */
    app_p->buf_p;
  cinfo_p->dest->free_in_buffer = /* set number of available buffer bytes */
    app_p->bufsize;
  }

/*******************************************************************
**
**   Local subroutine OUTPUT_WRITE (CINFO_P)
**
**   This routine is called by the JPEG library when the output data
**   buffer has been filled, and it must be written to the output stream.
*/
static boolean output_write (
  struct jpeg_compress_struct * cinfo_p) {

app_write_t * app_p;              /* pointer to application data */

/*
**   Start of executable code.
*/
  app_p = cinfo_p->client_data;   /* get pointer to application private data */

  app_p->bufn = app_p->bufsize;   /* set number of bytes to write */

  app_p->writebuf (app_p);        /* call application routine to write the buffer */

  cinfo_p->dest->next_output_byte = /* reset next byte pointer to buffer start */
    app_p->buf_p;
  cinfo_p->dest->free_in_buffer = /* reset number of available buffer bytes */
    app_p->bufsize;

  return TRUE;                    /* indicate whole buffer was written */
  }

/*******************************************************************
**
**   Local subroutine OUTPUT_END (CINFO_P)
**
**   This routine is called by the JPEG library when the last bit of the output
**   stream has been written to the output buffer.
*/
static void output_end (
  struct jpeg_compress_struct * cinfo_p) {

app_write_t * app_p;              /* pointer to application data */

/*
**   Start of executable code.
*/
  app_p = cinfo_p->client_data;   /* get pointer to application private data */

  app_p->bufn =                   /* compute the number of bytes in the buffer */
    app_p->bufsize - cinfo_p->dest->free_in_buffer;

  app_p->writebuf (app_p);        /* call application routine to write the buffer */
  }

/*******************************************************************
**
**   Subroutine JPG_OPEN_OUT (APP_P, QUAL, PIXX, PIXY, PIXASPECT, PIXFORM)
**
*/
void jpg_open_out (               /* open JPEG output stream */
  app_write_t * app_p,            /* pointer to application JPEG writing data */
  float qual,                     /* 0.0 to 1.0 quality level */
  int pixx,                       /* number of image pixels horizontally */
  int pixy,                       /* number of image pixel vertically */
  float pixaspect,                /* width/height aspect ratio of pixels */
  unsigned char pixform) {        /* pixel format ID */

struct jpeg_compress_struct *cinfo_p; /* points to main JPEG lib output structure */
struct jpeg_error_mgr * jerr_p;   /* points to error handling structure */
struct jpeg_destination_mgr * dest_p; /* points to output I/O structure */

/*
**   Start of executable code.
*/
  cinfo_p =                       /* allocate main JPEG lib output structure */
    malloc(sizeof(struct jpeg_compress_struct));
  jerr_p =                        /* allocate error handling descriptor */
    malloc(sizeof(struct jpeg_error_mgr));
  cinfo_p->err = jpeg_std_error(jerr_p); /* install default error handlers */
  jpeg_create_compress (cinfo_p); /* initialize compression stream info */

  cinfo_p->image_width = pixx;    /* set image dimensions in pixels */
  cinfo_p->image_height = pixy;
  switch (pixform) {              /* set values dependant on pixel format */
case jpeg_pixform_rgb24_k:
    cinfo_p->input_components = 3;
    cinfo_p->in_color_space = JCS_RGB;
    break;
case jpeg_pixform_gray8_k:
    cinfo_p->input_components = 1;
    cinfo_p->in_color_space = JCS_GRAYSCALE;
    break;
    };

  jpeg_set_defaults (cinfo_p);    /* set defaults based on above information */

  app_p->cinfo_p = cinfo_p;       /* save pointer to JPEG lib structure */
  cinfo_p->client_data = app_p;   /* set pointer to overall app structure */

  dest_p =                        /* allocate output I/O definition structure */
    malloc(sizeof(struct jpeg_destination_mgr));
  cinfo_p->dest = dest_p;         /* link from main output structure */

  jpeg_set_quality (              /* set compression/quality tradeoff */
    cinfo_p,                      /* pointer to compression stream state */
    (int)(qual * 100.0 + 0.5),    /* 0 to 100 relative quality level */
    TRUE);                        /* force baseline standard adherance */
  cinfo_p->dct_method = JDCT_FLOAT; /* use floating point arithmetic */

  if (pixaspect >= 1.0)
    {                             /* pixels are wider than tall */
      cinfo_p->Y_density = 60000;
      cinfo_p->X_density = (int)(cinfo_p->Y_density / pixaspect + 0.5);
      }
    else {                        /* pixels are taller than wide */
      cinfo_p->X_density = 60000;
      cinfo_p->Y_density = (int)(cinfo_p->X_density * pixaspect + 0.5);
      }
    ;

  dest_p->init_destination = output_init; /* install our output handler routines */
  dest_p->empty_output_buffer = output_write;
  dest_p->term_destination = output_end;

  jpeg_start_compress (cinfo_p, TRUE); /* initialize compressions and write header */
  }

/*******************************************************************
**
**   Subroutine JPG_WRITE_SCAN (APP_P, SCAN_P)
**
**   Write the scan line pointed to by SCAN_P to the JPEG output stream.
*/
void jpg_write_scan (             /* write scan line to JPEG output stream */
  app_write_t * app_p,            /* pointer to application JPEG writing state */
  void * scan_p) {                /* pointer to start of scan line data */

JSAMPROW scanptrs[1];             /* array of scan line pointers */

/*
**   Start of executable code.
*/
  scanptrs[0] = scan_p;           /* set pointer to the first and only row */
  jpeg_write_scanlines (app_p->cinfo_p, scanptrs, 1); /* write this one scan line */
  }

/*******************************************************************
**
**   Subroutine JPG_CLOSE_OUT (APP_P)
**
**   Close this JPEG library output stream and deallocate all resources
**   allocated by JPG_OPEN_OUT.
*/
void jpg_close_out (              /* close this JPEG library output stream */
  app_write_t * app_p) {          /* pointer to application JPEG writing state */

/*
**   Start of executable code.
*/
  jpeg_finish_compress (app_p->cinfo_p); /* handle last buffer full of data */

  free (app_p->cinfo_p->dest);    /* deallocate output writing state */
  free (app_p->cinfo_p->err);     /* deallocate error handling state */
  jpeg_destroy_compress (app_p->cinfo_p); /* deallocate implicit resources */
  free (app_p->cinfo_p);          /* deallocate JPEG library structure */
  app_p->cinfo_p = NULL;          /* indicate JPEG lib structure not allocated */
  }

/*******************************************************************
**
**   Local subroutine INPUT_INIT (CINFO_P)
**
**   This routine is called by the JPEG library once before the first
**   attempt to read data from the buffer.
*/
static void input_init (
  j_decompress_ptr cinfo_p) {

/*
**   Start of executable code.
*/
  cinfo_p->src->bytes_in_buffer = 0; /* indicate the input buffer is empty */
  }

/*******************************************************************
**
**   Local subroutine INPUT_READ (CINFO_P)
**
**   This routine is called by the JPEG library whenever the input buffer
**   has been exhausted and the library wants more data.  This routine must
**   make at least one unread byte available in the input buffer.
*/
static boolean input_read (
  j_decompress_ptr cinfo_p) {

app_read_t * app_p;               /* pointer to private app state */

/*
**   Start of executable code.
*/
  app_p = cinfo_p->client_data;   /* get pointer to private app state */
  app_p->readbuf (app_p);         /* read the next buffer full */
  cinfo_p->src->bytes_in_buffer = app_p->bufn; /* pass back num bytes now in buf */
  cinfo_p->src->next_input_byte = app_p->buf_p; /* pass back pointer to first byte */
  return TRUE;                    /* indicate success */
  }

/*******************************************************************
**
**   Local subroutine INPUT_SKIP (CINFO_P, N)
**
**   This routine is called by the JPEG library to skip over N bytes of
**   input data.
*/
static input_skip (
  j_decompress_ptr cinfo_p,
  long n) {                       /* number of bytes to skip, may be <= 0 */

app_read_t * app_p;               /* pointer to private app state */
sys_int_adr_t nchunk;             /* number of bytes to skip this chunk */

/*
**   Start of executable code.
*/
  app_p = cinfo_p->client_data;   /* get pointer to private app state */

  while (n > 0) {                 /* keep going until skipped all requested bytes */
    if (! cinfo_p->src->bytes_in_buffer) { /* buffer is empty ? */
      app_p->readbuf (app_p);     /* read more bytes into the buffer */
      cinfo_p->src->bytes_in_buffer = app_p->bufn; /* update JPEG lib buffer info */
      cinfo_p->src->next_input_byte = app_p->buf_p;
      }
    nchunk =                      /* make size of chunk to skip this time */
      (n < cinfo_p->src->bytes_in_buffer) ? n : cinfo_p->src->bytes_in_buffer;
    cinfo_p->src->bytes_in_buffer = cinfo_p->src->bytes_in_buffer - nchunk;
    cinfo_p->src->next_input_byte = cinfo_p->src->next_input_byte + nchunk;
    n = n - nchunk;               /* update number of bytes left to skip */
    }                             /* back for next chunk until all bytes skipped */
  }

/*******************************************************************
**
**   Local subroutine INPUT_END (CINFO_P)
**
**   This routine is called by the JPEG library after it is all done reading
**   the input stream.
**
**   This version does nothing.
*/
static input_end (
  j_decompress_ptr cinfo_p) {
  }

/*******************************************************************
**
**   Subroutine JPG_OPEN_IN (APP_P)
**
*/
void jpg_open_in (                /* open JPEG output stream */
  app_read_t * app_p) {           /* pointer to application JPEG reading state */

struct jpeg_decompress_struct * cinfo_p; /* points to main JPEG lib input structure */
struct jpeg_error_mgr * jerr_p;   /* points to error handling structure */
struct jpeg_source_mgr * src_p;   /* points to input I/O structure */

/*
**   Start of executable code.
*/
  cinfo_p =                       /* allocate main JPEG lib output structure */
    malloc(sizeof(struct jpeg_decompress_struct));
  jerr_p =                        /* allocate error handling descriptor */
    malloc(sizeof(struct jpeg_error_mgr));
  cinfo_p->err = jpeg_std_error(jerr_p); /* install default error handlers */
  jpeg_create_decompress (cinfo_p); /* initialize decompression stream info */

  app_p->cinfo_p = cinfo_p;       /* save pointer to JPEG lib structure */
  cinfo_p->client_data = app_p;   /* set pointer to overall app structure */

  src_p =                         /* allocate input I/O definition structure */
    malloc(sizeof(struct jpeg_source_mgr));
  cinfo_p->src = src_p;           /* link from main output structure */
  src_p->init_source = input_init; /* install our input handler routines */
  src_p->fill_input_buffer = input_read;
  src_p->skip_input_data = input_skip;
  src_p->resync_to_restart = jpeg_resync_to_restart;
  src_p->term_source = input_end;

  jpeg_read_header (cinfo_p, TRUE); /* read file header and get image parameters */

  cinfo_p->dct_method = JDCT_FLOAT; /* use floating point arithmetic */
  cinfo_p->out_color_space = JCS_RGB; /* return pixel values in RGB format */
  app_p->pixform = jpeg_pixform_rgb24_k; /* pass back what pixel format will be */
  app_p->ncomp = 3;

  jpeg_start_decompress (cinfo_p); /* get ready for returning uncompress pixels */

  app_p->pixx = cinfo_p->output_width; /* return image parameters */
  app_p->pixy = cinfo_p->output_height;
  }

/*******************************************************************
**
**   Subroutine JPG_READ_SCAN (APP_P, BUF_P)
**
**   Read the next uncompressed scan line from the JPEG stream into the
**   the buffer pointed to by BUF_P.
*/
void jpg_read_scan (              /* read next uncompressed scan line */
  app_read_t * app_p,             /* pointer to application JPEG reading state */
  void * buf_p) {                 /* pointer to the start of the buffer */

JSAMPROW scanptrs[1];             /* array of scan line pointers */

/*
**   Start of executable code.
*/
  scanptrs[0] = buf_p;            /* set pointer to the one and only scan line */
  jpeg_read_scanlines (           /* read scan line into our buffer */
    app_p->cinfo_p,               /* pointer to JPEG library reading state */
    scanptrs,                     /* array of pointers to scan line buffers */
    1);                           /* max number of scan lines to read */
  }

/*******************************************************************
**
**   Subroutine JPG_CLOSE_IN (APP_P)
**
**   Close this JPEG input stream.  We've already read all the decompressed
**   input scan lines we care about, so the only thing left to have the JPEG
**   library do is to release all its resources associated with this
**   stream.  There is no point in shutting down "politely".  We simply
**   abort.
*/
void jpg_close_in (               /* close JPEG input stream, deallocate resources */
  app_read_t * app_p) {           /* pointer to application JPEG reading state */

struct jpeg_decompress_struct * cinfo_p; /* points to main JPEG lib input structure */

/*
**   Start of executable code.
*/
  cinfo_p = app_p->cinfo_p;       /* get pointer to JPEG library stream state */

  jpeg_destroy_decompress (cinfo_p); /* abort input, release JPEG lib resources */

  free (cinfo_p->src);            /* deallocate input I/O structure */
  free (cinfo_p->err);            /* deallocate error handler structure */
  free (cinfo_p);                 /* deallocate JPEG library input stream state */
  app_p->cinfo_p = NULL;          /* indicate JPEG library structure not allocated */
  }
