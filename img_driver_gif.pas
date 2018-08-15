{   IMG library driver for handling GIF image files.  GIF stands for
*   Graphics Interchange Format, and is a registered service mark of
*   Compuserve, Inc. of Columbus Ohio.
}
module img_driver_gif;
define img_d_gif_open_write;
define img_d_gif_open_read;
%include '/cognivision_links/dsee_libs/img/img2.ins.pas';
{
*******************************************************************
*******************************************************************
*
*   This section contains declarations that are common to both
*   reading and writing GIF image files.
}
const
  block_image_k = 16#2C;               {block ID for image descriptor}
  block_ext_k = 16#21;                 {block ID for an extension}
  block_trail_k = 16#3B;               {block ID for trailer}
  ext_gcont_k = 16#F9;                 {graphics control extension ID}
  ext_comm_k = 16#FE;                  {comment extension ID}
  ext_text_k = 16#01;                  {plain text extension ID}
  ext_app_k = 16#FF;                   {application extension ID}
{
*******************************************************************
*******************************************************************
*
*   This section contains information that is unique to writing
*   GIF files.
}
const
  write_buf_size_k = 2048;             {bytes written at one time to output file}
  write_buf_last_k =                   {last valid write buffer index}
    write_buf_size_k - 1;
  write_n_red_k = 6;                   {number of different red levels}
  write_n_grn_k = 8;                   {number of different green levels}
  write_n_blu_k = 5;                   {number of different blue levels}
  dith_frac_high_k = 1024;             {FRAC value to indicate DITH_HIGH value}

type
{
*   Each WRITE_LZW_T block is one node in the LZW code tree that can have up
*   to 16 child nodes.  Each tree level decodes an additional 4 bits of input
*   code.  Each block is always the same size.  Blocks are allocated once and
*   re-used instead of deallocated.  Free and used blocks are kept on separate
*   chains.  The NEXT_P field points to the next block in the chain.  A value
*   of NIL indicates end of chain.  When the code table is cleared, all the
*   used blocks are simply added to the start of the free chain.  The NEXT_P
*   field in the root node points to the start of the used chain.
}
  write_lzw_p_t = ^write_lzw_t;
  write_lzw_t = record                 {uniform blocks in code tree}
    next_p: write_lzw_p_t;             {pointer to next block in chain}
    next:                              {pointers to all the possible child nodes}
      array[0..15] of write_lzw_p_t;   {NIL indicates end of tree for this code}
    code:                              {LZW code values at this point in tree}
      array[0..15] of sys_int_min12_t; {0 indicates no LZW code value exists here}
    end;

  write_pix_t = record                 {one original saved pixel value}
    alpha: sys_int_min16_t;            {0-255 is normal pixel value}
    red: sys_int_min16_t;
    grn: sys_int_min16_t;
    blu: sys_int_min16_t;
    end;
  write_pix_p_t = ^write_pix_t;

  dith_entry_t = record                {one dither table entry for one color}
    close: sys_int_machine_t;          {component value for closest single approx}
    frac: sys_int_machine_t;           {fraction into range from low to high value}
    high: sys_int_machine_t;           {higher of two dither choices}
    low: sys_int_machine_t;            {lower of two dither choices}
    end;

  dith_col_t =                         {all the dither table entries for one color}
    array[0..255] of dith_entry_t;

  dith_t = record                      {the dither table entries for all the colors}
    red: dith_col_t;
    grn: dith_col_t;
    blu: dith_col_t;
    end;

  write_conv_k_t = (                   {methods for converting to GIF pixel values}
    write_conv_pick_k,                 {pick closest available color}
    write_conv_4x4_k,                  {4x4 dither pattern}
    write_conv_gray_k);                {gray scale only}

  data_write_t = record                {private connection data for writing GIF file}
    img_conn_p: img_conn_p_t;          {pointer to public image connection block}
    img_conn2_p: img_conn2_p_t;        {pointer to internal IMG library conn block}
    conn: file_conn_t;                 {handle to GIF file open for binary write}
    animsec: real;                     {seconds to display image within animation}
    transparent: boolean;              {TRUE if alpha component used}
    anim: boolean;                     {TRUE if GIF file holds animation sequence}
    loop: boolean;                     {TRUE if GIF is animation and auto loops}
    conv: write_conv_k_t;              {true color to GIF pixels convert method}
    pix_src_p: write_pix_p_t;          {pnt to first pixel in saved source image}
    size_x, size_y: sys_int_machine_t; {image size in pixels}
    pix_src_dy: sys_int_adr_t;         {mem stride for each source scan down}
    lut:                               {translates GIF pixel values to RGBA}
      array[0..255] of img_pixel1_t;
    nlut: sys_int_machine_t;           {number of valid LUT entries}
    dith: dith_t;                      {table for finding pseudo colors from true}
    pix_gif_dy: sys_int_adr_t;         {mem stride for each GIF scan line down}
    pix_nbits: sys_int_machine_t;      {number of bits needed for raw out pixels}
    lzw_free_p: write_lzw_p_t;         {points to first block in free chain}
    lzw_last_p: write_lzw_p_t;         {points to last block of used chain}
    lzw_root: write_lzw_t;             {root of LZW code tree}
    lzw_next: sys_int_machine_t;       {next code to use for new table entry}
    lzw_left_size: sys_int_machine_t;  {num new codes left at this code bit size}
    lzw_code: sys_int_machine_t;       {LZW code for current string}
    lzw_len: sys_int_machine_t;        {number of pixels in current accumulated str}
    lzw_p: write_lzw_p_t;              {points to current terminal code block}
    lzw_ind: sys_int_machine_t;        {0-15 index within curr terminal code block}
    code_nbits: sys_int_machine_t;     {current LZW code size in bits}
    code_n: sys_int_machine_t;         {number of bits in CODE_BUF}
    code_buf: sys_int_machine_t;       {partial LZW stream byte in low bits}
    data_n: sys_int_machine_t;         {number of data bytes in DATA_BUF}
    buf_n: sys_int_adr_t;              {number of bytes in BUF}
    pix_gif_p: ^char;                  {pnt to first pixel in array of GIF pixels}
    data_buf: array[0..254] of char;   {data bytes in block being currently written}
    buf: array[0..write_buf_last_k] of char; {buffer for writing to GIF file}
    end;
  data_write_p_t = ^data_write_t;      {pointer to our private GIF writing data}
{
****************************************************
*
*   Subroutine WRITE_FLUSH (D)
*
*   Write all data in the output buffer to the GIF file.  The output buffer
*   is reset to empty.  Nothing will be done if the output buffer is already
*   empty.  This routine must be called right before the output file is
*   closed.
}
procedure write_flush (                {write output buffer to file}
  in out  d: data_write_t);            {our private GIF writing state block}
  internal;

var
  stat: sys_err_t;

begin
  if d.buf_n > 0 then begin            {the output buffer is not empty ?}
    file_write_bin (d.buf, d.conn, d.buf_n, stat); {write the buffer to the GIF file}
    sys_error_abort (stat, 'img', 'gif_write', nil, 0);
    end;

  d.buf_n := 0;                        {reset output buffer to empty}
  end;
{
****************************************************
*
*   Subroutine WRITE_BYTE (D, B)
*
*   Write one byte to the output stream.  Note that the output stream is
*   buffered.  This subroutine deals with the buffer and presents a simple
*   interface for writing a single byte.
*
*   Note on buffer useage:  The output buffer in D.BUF should never be
*     completely full, except inside this routine.  We always empty the
*     buffer as soon as it is filled.  We can therefore always write
*     one byte before checking for room in the buffer.  All other data
*     output routines eventually funnel thru this routine, so this is
*     the only place the buffering logic lives.  The only exception is
*     that higher routines may call WRITE_FLUSH at any time.  WRITE_FLUSH
*     must be called before the output file is closed.
}
procedure write_byte (                 {write one byte to output stream}
  in out  d: data_write_t;             {our private GIF writing state block}
  in      b: sys_int_machine_t);       {byte value in low 8 bits}
  val_param; internal;

begin
  d.buf[d.buf_n] := chr(b);            {stuff the byte into the output buffer}
  d.buf_n := d.buf_n + 1;              {indicate one more byte in the buffer}

  if d.buf_n >= write_buf_size_k then begin {we just filled the buffer ?}
    write_flush (d);                   {write the buffer and reset it to empty}
    end;
  end;
{
****************************************************
*
*   Subroutine WRITE_I16 (D, I)
*
*   Write the 16 bit integer value in the low bits of I to the output
*   stream.
}
procedure write_i16 (                  {write 16 bit integer to the output stream}
  in out  d: data_write_t;             {our private GIF writing state block}
  in      i: sys_int_machine_t);       {16 bit integer value in low bits}
  val_param; internal;

begin
  write_byte (d, i & 255);             {write the low byte}
  write_byte (d, rshft(i, 8) & 255);   {write the high byte}
  end;
{
****************************************************
*
*   Subroutine WRITE_DATA_FLUSH (D)
*
*   Write the current GIF data sub block to the output stream and reset
*   to no current sub block started.  This routine will have no effect if
*   the current sub block is empty.
}
procedure write_data_flush (           {write current sub block, reset to empty}
  in out  d: data_write_t);            {our private GIF writing state block}
  internal;

var
  i: sys_int_machine_t;                {scratch loop counter}

begin
  if d.data_n > 0 then begin           {there is stuff in the current sub block ?}
    write_byte (d, d.data_n);          {write block size byte}
    for i := 0 to d.data_n-1 do begin  {once for each byte in sub block}
      write_byte (d, ord(d.data_buf[i]));
      end;
    end;

  d.data_n := 0;                       {reset to no data in the current sub block}
  end;
{
****************************************************
*
*   Subroutine WRITE_DATA_BYTE (D, B)
*
*   Write an additional byte to the data stream encapsulated into a sequence
*   of GIF data sub-blocks.  If the current sub block is filled, then it is
*   written to the output stream and a new empty sub block started.
}
procedure write_data_byte (            {write byte to GIF data sub block stream}
  in out  d: data_write_t;             {our private GIF writing state block}
  in      b: sys_int_machine_t);       {byte value in low 8 bits}
  val_param; internal;

begin
  d.data_buf[d.data_n] := chr(b);      {stuff the byte into the sub block buffer}
  d.data_n := d.data_n + 1;            {count one more byte in current sub block}

  if d.data_n >= 255 then begin        {we just filled the current sub block ?}
    write_data_flush (d);              {write the sub block and reset it to empty}
    end;
  end;
{
****************************************************
*
*   Subroutine WRITE_DATA_END (D)
*
*   Indicate the end of a data stream encapsulated in GIF data sub blocks.
*   The current sub block will be closed and written (if any) and the
*   block terminator will be written to indicate the end of this sequence
*   of sub blocks.
}
procedure write_data_end (             {close data stream of sub blocks}
  in out  d: data_write_t);            {our private GIF writing state block}
  internal;

begin
  write_data_flush (d);                {finish the current sub block, if any}
  write_byte (d, 0);                   {write GIF block terminator}
  end;
{
****************************************************
*
*   Subroutine WRITE_LZW_CODE (D, CODE)
*
*   Write one LZW code to the LZW code stream.  The code is in the low bits
*   of CODE.  The number of bits in code is the current LZW code size, which
*   is kept in D.CODE_NBITS.
}
procedure write_lzw_code (             {write one LZW code to the LZW stream}
  in out  d: data_write_t;             {our private GIF writing state block}
  in      code: sys_int_machine_t);    {LZW code to write in low bits}
  val_param; internal;

var
  bits_left: sys_int_machine_t;        {bits left to write in CD}
  cd: sys_int_machine_t;               {bits left to be written aligned in LSB}
  bt: sys_int_machine_t;               {bits to use this time around loop}
  mask: sys_int_machine_t;             {mask for bits this time around loop}

begin
  bits_left := d.code_nbits;           {init bits left to write to data stream}
  cd := code;                          {init the data bits left to write}

  while bits_left > 0 do begin         {loop until all bits written}
    bt := min(bits_left, 8 - d.code_n); {num of bits to write this time around loop}
    mask := ~lshft(~0, bt);            {for masking in bits to transfer this time}
    d.code_buf :=                      {merge new bits into one byte buffer}
      d.code_buf ! lshft(cd & mask, d.code_n);
    d.code_n := d.code_n + bt;         {update number of bits in one byte buffer}
    cd := rshft(cd, bt);               {remove the bits we just wrote}
    bits_left := bits_left - bt;       {update the number of bits left in CD}

    if d.code_n >= 8 then begin        {the one byte buffer is full now ?}
      write_data_byte (d, d.code_buf); {write the full byte to the output stream}
      d.code_n := 0;                   {reset one byte buffer to empty}
      d.code_buf := 0;                 {reset all buffer bits to not set}
      end;
    end;                               {back to handle next chunk of bits in CD}
  end;
{
****************************************************
*
*   Subroutine W_LZW_CLEAR (D)
*
*   Perform the LZW clear operation.
}
procedure w_lzw_clear (                {preform LZW clear in GIF writer}
  in out  d: data_write_t);            {our private GIF writing state block}
  internal;

var
  i: sys_int_machine_t;                {scratch loop counter}

begin
  if d.lzw_root.next_p <> nil then begin {we have some used code blocks ?}
    d.lzw_last_p^.next_p := d.lzw_free_p; {point last used block to first free block}
    d.lzw_free_p := d.lzw_root.next_p; {everything is now on the free chain}
    d.lzw_root.next_p := nil;          {indicate no code blocks currently in use}
    d.lzw_last_p := nil;
    end;

  for i := 0 to 15 do begin            {once for each code block split entry}
    d.lzw_root.next[i] := nil;         {no further tree nodes for this code split}
    d.lzw_root.code[i] := 0;           {no LZW code exists for this split}
    end;

  d.lzw_next := lshft(1, d.pix_nbits) + 2; {reset next LZW code that will be defined}
  d.lzw_left_size :=                   {number of codes left at this code size}
    lshft(1, d.pix_nbits+1) - d.lzw_next;
  d.lzw_code := 0;                     {no current LZW code exists}
  d.lzw_len := 0;                      {the accumulated string is empty}
  d.lzw_p := nil;                      {no current string exists}
  d.lzw_ind := 0;                      {no current block to index into}
  d.code_nbits := d.pix_nbits + 1;     {reset size of next LZW code}
  end;
{
****************************************************
*
*   Subroutine WRITE_LZW_CLEAR (D)
*
*   Write the LZW clear code to the current LZW code stream.  The LZW
*   encoder will also be reset.
}
procedure write_lzw_clear (            {write the LZW clear code to the LZW stream}
  in out  d: data_write_t);            {our private GIF writing state block}
  internal;

begin
  write_lzw_code (d, lshft(1, d.pix_nbits)); {write the clear code}
  w_lzw_clear (d);                     {reset the encoder}
  end;
{
****************************************************
*
*   Subroutine WRITE_LZW_END (D)
*
*   Indicate the end of the current LZW code stream.  The LZW end code will
*   be written and the sequence of data blocks containing the LZW code stream
*   will be closed.
}
procedure write_lzw_end (              {finish and close LZW code stream}
  in out  d: data_write_t);            {our private GIF writing state block}
  internal;

begin
  if d.lzw_len > 0 then begin          {unwritten accumulated string exists ?}
    write_lzw_code (d, d.lzw_code);    {write the code for the pending string}
    end;

  write_lzw_code (d, lshft(1, d.pix_nbits) + 1); {write the end of information code}

  if d.code_n > 0 then begin           {partial byte not yet written ?}
    write_data_byte (d, d.code_buf);   {write the partial byte}
    d.code_n := 0;                     {indicate no unwritten bits in buffer}
    d.code_buf := 0;                   {reset all buffer bits to not set}
    end;

  write_data_end (d);                  {close the GIF data stream of data blocks}
  end;
{
****************************************************
*
*   Function W_NODE_NEW (D)
*
*   Allocate and initialize a new LZW code tree node.  The node is taken from
*   the free list, if possible.
}
function w_node_new (                  {allocate and init a new tree node}
  in out  d: data_write_t)             {our private GIF writing state block}
  :write_lzw_p_t;                      {returned pointer to the new tree node}
  internal;

var
  p: write_lzw_p_t;                    {pointer to new node}
  i: sys_int_machine_t;                {scratch loop counter}

begin
  if d.lzw_free_p = nil
    then begin                         {free list is empty, allocate new node}
      util_mem_grab (                  {allocate memory for the new node}
        sizeof(p^),                    {amount of memory to grab}
        d.img_conn2_p^.mem_p^,         {parent memory context}
        false,                         {we won't individually deallocate this mem}
        p);                            {returned pointer to the new memory}
      end
    else begin                         {re-use a free list entry}
      p := d.lzw_free_p;               {get pointer to first node on the free list}
      d.lzw_free_p := p^.next_p;       {next node is now start of free list}
      end
    ;
{
*   P is pointing to the new node, regardless of where it came from.
*
*   Now add this new node to the start of the used chain.
}
  if d.lzw_root.next_p = nil
    then begin                         {this is the first node on the used chain ?}
      d.lzw_last_p := p;               {this will be the last used chain entry}
      p^.next_p := nil;                {indicate this node is the end of the chain}
      end
    else begin                         {there is a previous entry in the chain}
      p^.next_p := d.lzw_root.next_p;  {point new node to old head of chain}
      end
    ;
  d.lzw_root.next_p := p;              {the new node is now the start of the chain}
{
*   Initialize the values of the new node.  NEXT_P has already been set.
}
  for i := 0 to 15 do begin            {once for each 0-15 split number}
    p^.next[i] := nil;                 {indicate tree here for this split}
    p^.code[i] := 0;                   {indicate no LZW code for accumulated string}
    end;

  w_node_new := p;                     {pass back pointer to the new node}
  end;
{
****************************************************
*
*   Function W_CODE_NEW (D)
*
*   Return the LZW code to use for a new table entry.  This routine
*   automatically handles sending a clear when the next code would
*   exceed the maximum allowable code of 4095.  In that case, the function
*   value is negative and no new code is allocated.
}
function w_code_new (                  {get new LZW code for next table entry}
  in out  d: data_write_t)             {our private GIF writing state block}
  :sys_int_machine_t;                  {returned new LZW code value}
  internal;

begin
  if d.lzw_left_size = 0 then begin    {exhausted all the codes at this bit size ?}
    if d.code_nbits >= 12
      then begin                       {we are already at largest allowed code size}
        write_lzw_clear (d);           {clear tables and reset to initial code}
        w_code_new := -1;              {indicate tables reset, no new code returned}
        return;
        end
      else begin                       {move up to next larger code size}
        d.code_nbits := d.code_nbits + 1; {codes will now be one bit wider}
        d.lzw_left_size := d.lzw_next; {note this is always a power of 2}
        end
      ;
    end;

  w_code_new := d.lzw_next;            {pass back value of next code}
  d.lzw_next := d.lzw_next + 1;        {make code value to pass back next time}
  d.lzw_left_size := d.lzw_left_size - 1; {count one less code left at this size}
  end;
{
****************************************************
*
*   Subroutine WRITE_PIX (D, PIX)
*
*   Write the next sequential image pixel to the output stream.  The
*   pixels will be LZW compressed and encoded into the output stream as
*   per the GIF spec.  PIX is the raw pixel value.  It *MUST* be in the
*   range of 0 to 2**d.pix_nbits-1.  For example, if PIX_NBITS is 8, then
*   PIX must be in the range of 0-255.  The PIX value is not checked, but
*   out of range values can wreak havoc.
}
procedure write_pix (                  {write next pixel to output stream}
  in out  d: data_write_t;             {our private GIF writing state block}
  in      pix: sys_int_machine_t);     {0 to 2**PIX_NBITS-1 GIF pixel value}
  val_param; internal;

var
  bits_left: sys_int_machine_t;        {bits left to work up tree for this PIX value}
  pixb: sys_int_machine_t;             {remaining BITS_LEFT to do aligned in LSB}
  bt: sys_int_machine_t;               {bits to use this tree level}
  mask: sys_int_machine_t;             {mask for current split bits in PIXB}
  code: sys_int_machine_t;             {scratch LZW code value}

label
  new_string;

begin
  bits_left := d.pix_nbits;            {init bits left to do for this pixel}
  pixb := pix;                         {init buffer with the bits left to do}
{
*   Handle case where this is the first code of a new string.
}
  if d.lzw_len = 0 then begin          {this pixel value starts a new string ?}
new_string:                            {jump here if PIX starts a new string}
    d.lzw_p := addr(d.lzw_root);       {point to the root LZW code tree node}
    bt := min(4, bits_left);           {number of bits for this split}
    bits_left := bits_left - bt;       {fewer bits from bit buffer left to go}
    mask := ~lshft(~0, bt);            {make mask for split bits in PIXB}
    d.lzw_ind := pixb & mask;          {extract bits for this split}
    pixb := rshft(pixb, bt);           {remove these split bits from bit buffer}
    end;
{
*   Loop back here until all the original pixel value bits have been used
*   to find the terminating LZW code tree node for the accumulated string.
}
  while bits_left > 0 do begin         {back here as long as bits left to process}
    if d.lzw_p^.next[d.lzw_ind] = nil then begin {no further tree node exists here ?}
      d.lzw_p^.next[d.lzw_ind] := w_node_new(d); {create new node where we are going}
      end;
    d.lzw_p := d.lzw_p^.next[d.lzw_ind]; {move to next tree node}
    bt := min(4, bits_left);           {number of bits for this split}
    bits_left := bits_left - bt;       {fewer bits from bit buffer left to go}
    mask := ~lshft(~0, bt);            {make mask for split bits in PIXB}
    d.lzw_ind := pixb & mask;          {extract bits for this split}
    pixb := rshft(pixb, bt);           {remove these split bits from bit buffer}
    end;                               {back to process next bits in this pix value}
{
*   We have arrived at the terminal node for the accumulated string with the
*   new pixel value added to the end.  LZW_P is pointing to the terminating
*   tree node, and LZW_IND is the 0-15 split index within that tree node.
}
  if d.lzw_len = 0 then begin          {this was the first code in a new string ?}
    d.lzw_code := pix;                 {set code for raw pixel value}
    d.lzw_len := 1;                    {accumulated string now has one pixel in it}
    return;                            {all ready for next pixel value}
    end;

  if d.lzw_p^.code[d.lzw_ind] = 0
    then begin                         {no code exists for new accumulated string}
      write_lzw_code (d, d.lzw_code);  {write code for string without this new pix}
      code := w_code_new(d);           {get code for next new table entry}
      if code > 0 then begin           {didn't just clear the code tables ?}
        d.lzw_p^.code[d.lzw_ind] := code; {assign new code for whole string}
        end;
      d.lzw_len := 0;                  {reset to accumulated string is empty}
      bits_left := d.pix_nbits;        {init bits left to do for this pixel}
      pixb := pix;                     {init buffer with the bits left to do}
      goto new_string;                 {back to start new string with this PIX value}
      end
    else begin                         {the accumulated string already has a code}
      d.lzw_code := d.lzw_p^.code[d.lzw_ind]; {update current code for whole string}
      d.lzw_len := d.lzw_len + 1;      {update new length of accumulated string}
      end
    ;
  end;
{
****************************************************
*
*   Subroutine WRITE_IMAGE (D)
*
*   Write the GIF pixel values at D.PIX_GIF_P to the output file as one
*   GIF image.
}
procedure write_image (                {write GIF pixels to output file as an image}
  in out  d: data_write_t);            {our private GIF writing state block}
  internal;

var
  i: sys_int_machine_t;                {scratch integer}
  npix: sys_int_machine_t;             {total number of pixels to write}
  p: ^char;                            {pointer to current GIF pixel value}

begin
{
*   Write Graphic Control Extension block if transparency or animation is
*   enabled.
}
  if d.transparent or d.anim then begin {need control extension block features ?}
    write_byte (d, block_ext_k);       {ID for extension block}
    write_byte (d, ext_gcont_k);       {ID for graphics control extension}

    i := 0;                            {init no disposal, no user input, no transp}
    if d.transparent then i := i ! 1;  {set transparency index valid flag}
    write_data_byte (d, i);            {write packed fields byte}

    i := 0;                            {init to no delay time specified}
    if d.anim then begin               {animation, we have specific delay time ?}
      i := round(d.animsec * 100.0);   {set delay time for this frame}
      i := min(65535, i);              {clip to legal max limit}
      end;
    write_data_byte (d, i & 255);      {write delay time low byte}
    write_data_byte (d, rshft(i, 8) & 255); {write delay time high byte}

    write_data_byte (d, d.nlut);       {pixel value for transparent (if enabled)}
    write_data_end (d);                {close graphic control extension data block}
    end;
{
*   Write GIF Image Descriptor.
}
  write_byte (d, block_image_k);       {identifies beginning of image descriptor}
  write_i16 (d, 0);                    {image top left corner within logical screen}
  write_i16 (d, 0);
  write_i16 (d, d.size_x);             {image size in pixels}
  write_i16 (d, d.size_y);
  i :=
    lshft(0, 7) !                      {no local color table for this image}
    lshft(0, 6) !                      {image is not interlaced}
    lshft(0, 5) !                      {local color table not sorted (unused)}
    0;                                 {Log2 - 1 local color table size (unused)}
  write_byte (d, i);                   {write packed fields byte}
{
*   Write the image data.
}
  w_lzw_clear (d);                     {reset the LZW compressor state}
  write_byte (d, d.pix_nbits);         {bit size of pixels represented by LZW codes}
  write_lzw_clear (d);                 {write the initial LZW clear code}

  npix := d.size_x * d.size_y;         {total number of pixels we will write}
  p := d.pix_gif_p;                    {init source pointer to first pixel in image}

  for i := 0 to npix-1 do begin        {once for each pixel to write out}
    write_pix (d, ord(p^));            {send this pixel to LZW compressor}
    p := univ_ptr(                     {advance to data for next pixel}
      sys_int_adr_t(p) + sizeof(p^));
    end;                               {back to process next pixel in the image}

  write_lzw_end (d);                   {close LZW data stream}
  end;
{
****************************************************
*
*   Subroutine W_CONV_PICK (D)
*
*   Process the saved input image data at D.PIX_SRC_P to create the GIF
*   output pixel values at D.PIX_GIF_P.  The input pixel data may get
*   trashed.
*
*   This routine converts to the GIF pixels by picking the closest available
*   output color value.
}
procedure w_conv_pick (                {make GIF pixel values from saved input image}
  in out  d: data_write_t);            {our private GIF writing state block}
  internal;

var
  src_p: write_pix_p_t;                {pointer to current saved source pixel}
  dst_p: ^char;                        {pointer to current GIF desination pixel}
  npix: sys_int_machine_t;             {total number of pixels in the image}
  i: sys_int_machine_t;                {loop counter}
  g: sys_int_machine_t;                {GIF pixel value}

begin
  npix := d.size_x * d.size_y;         {make total number of pixels in the image}
  src_p := d.pix_src_p;                {init pointer to first source pixel}
  dst_p := d.pix_gif_p;                {init pointer to first destination pixel}

  if d.transparent
    then begin                         {totally transparent LUT index exists}
      for i := 0 to npix-1 do begin    {once for each pixel in the image}
        if src_p^.alpha >= 128
          then begin                   {make the pixel opaque}
            g :=                       {make GIF pseudo color pixel value}
              d.dith.red[src_p^.red].close +
              d.dith.grn[src_p^.grn].close +
              d.dith.blu[src_p^.blu].close;
            dst_p^ := chr(g);          {save GIF pixel value in GIF image array}
            end
          else begin                   {make the pixel transparent}
            dst_p^ := chr(d.nlut);
            end
          ;
        src_p := univ_ptr(             {advance to next source pixel}
          sys_int_adr_t(src_p) + sizeof(src_p^));
        dst_p := univ_ptr(             {advance to next destination pixel}
          sys_int_adr_t(dst_p) + sizeof(dst_p^));
        end;                           {back for next pixel in image}
      end
    else begin                         {all pixels will be opaque}
      for i := 0 to npix-1 do begin    {once for each pixel in the image}
        g :=                           {make GIF pseudo color pixel value}
          d.dith.red[src_p^.red].close +
          d.dith.grn[src_p^.grn].close +
          d.dith.blu[src_p^.blu].close;
        dst_p^ := chr(g);              {save GIF pixel value in GIF image array}
        src_p := univ_ptr(             {advance to next source pixel}
          sys_int_adr_t(src_p) + sizeof(src_p^));
        dst_p := univ_ptr(             {advance to next destination pixel}
          sys_int_adr_t(dst_p) + sizeof(dst_p^));
        end;                           {back for next pixel in image}
      end
    ;
  end;
{
****************************************************
*
*   Subroutine W_CONV_4x4 (D)
*
*   Process the saved input image data at D.PIX_SRC_P to create the GIF
*   output pixel values at D.PIX_GIF_P.  The input pixel data may get
*   trashed.
*
*   This routine applies a 4x4 dither pattern to gain more color resolution.
}
var
  dith_4x4:                            {array of 4x4 either pattern threshold values}
    array[0..3, 0..3] of sys_int_machine_t := [
      [ 1,  5,  9, 13],
      [ 3,  7, 11, 15],
      [10, 14,  2,  6],
      [12, 16,  4,  8]
    ];

procedure w_conv_4x4 (                 {make GIF pixel values from saved input image}
  in out  d: data_write_t);            {our private GIF writing state block}
  internal;

var
  src_p: write_pix_p_t;                {pointer to current saved source pixel}
  dst_p: ^char;                        {pointer to current GIF desination pixel}
  npix: sys_int_machine_t;             {total number of pixels in the image}
  i, j: sys_int_machine_t;             {scratch integers and loop counters}
  g: sys_int_machine_t;                {GIF pixel value}
  xd, yd: sys_int_machine_t;           {0-3 dither pattern indicies}
  x, y: sys_int_machine_t;             {X, Y pixel coordinate}
  red, grn, blu, alpha: sys_int_machine_t; {scratch pixel component values}
  dith:                                {dither thresholds for RGB}
    array[0..3, 0..3] of sys_int_machine_t;
  dith_alpha:                          {dither thresholds for ALPHA}
    array[0..3, 0..3] of sys_int_machine_t;

label
  next_dith_alpha, transp;

begin
{
*   Build the actual dither tables from the dither pattern array.
*   The values in the dither pattern array are numbered 1-16.  The values
*   in the dither final dither tables must have a range of 0-DITH_FRAC_HIGH_K.
*
*   Build the RGB dither table.  This is just a simple scaling from the
*   dither pattern array.
}
  for yd := 0 to 3 do begin            {down the dither rows}
    for xd := 0 to 3 do begin          {across this dither row}
      dith[yd, xd] := (dith_4x4[yd, xd] * dith_frac_high_k) div 16;
      end;
    end;
{
*   Build the ALPHA dither table.  Alpha values fade out from the extremes
*   toward the mean threshold values.
}
  for i := 1 to 16 do begin            {once for each pattern value}
    if (i & 1) <> 0
      then begin                       {odd value, start at low end}
        j := i;
        end
      else begin                       {even value, start at high end}
        j := 18 - i;
        end
      ;                                {J is 1-16 thresh where stick this value}
    for yd := 0 to 3 do begin          {down the dither rows}
      for xd := 0 to 3 do begin        {across this dither row}
        if dith_4x4[yd, xd] = j then begin {found the right dither table position ?}
          j := round((17 - j) * 256.0 / 17.0); {make 0-255 threshold value}
          dith_alpha[yd, xd] := j;     {set alpha dither threshold value here}
          goto next_dith_alpha;        {no point searching further}
          end;
        end;
      end;
next_dith_alpha:
    end;
{
*   Init before running thru all the pixels.
}
  npix := d.size_x * d.size_y;         {make total number of pixels in the image}
  src_p := d.pix_src_p;                {init pointer to first source pixel}
  dst_p := d.pix_gif_p;                {init pointer to first destination pixel}
  yd := 0;                             {init to at top row of dither pattern}

  if d.transparent then goto transp;   {image may not be totally opaque ?}
{
*   The output pixels will be totally opaque.
}
  for y := 0 to d.size_y-1 do begin    {down the scan lines}
    xd := 0;                           {reset to left edge of dither pattern}
    for x := 0 to d.size_x-1 do begin  {accross this scan line}
      if d.dith.red[src_p^.red].frac >= dith[yd, xd]
        then g := d.dith.red[src_p^.red].high
        else g := d.dith.red[src_p^.red].low;
      if d.dith.grn[src_p^.grn].frac >= dith[yd, xd]
        then g := g + d.dith.grn[src_p^.grn].high
        else g := g + d.dith.grn[src_p^.grn].low;
      if d.dith.blu[src_p^.blu].frac >= dith[yd, xd]
        then g := g + d.dith.blu[src_p^.blu].high
        else g := g + d.dith.blu[src_p^.blu].low;
      dst_p^ := chr(g);                {save GIF pixel value in GIF image array}
      xd := xd + 1;                    {across this dither pattern row}
      if xd > 3 then xd := 0;          {wrap back to start of row ?}
      src_p := univ_ptr(               {advance to next source pixel}
        sys_int_adr_t(src_p) + sizeof(src_p^));
      dst_p := univ_ptr(               {advance to next destination pixel}
        sys_int_adr_t(dst_p) + sizeof(dst_p^));
      end;                             {back for next pixel in this scan line}
    yd := yd + 1;                      {to the next dither pattern row down}
    if yd > 3 then yd := 0;            {wrap back to the top row ?}
    end;                               {back for next scan line in image}
  return;
{
*   The output pixels may be transparent.
}
transp:
  for y := 0 to d.size_y-1 do begin    {down the scan lines}
    xd := 0;                           {reset to left edge of dither pattern}
    for x := 0 to d.size_x-1 do begin  {accross this scan line}
      alpha := src_p^.alpha;           {fetch alpha pixel component}
      if alpha >= dith_alpha[yd, xd]
        then begin                     {make this pixel opaque}
          red := src_p^.red;           {fetch pixel color components}
          grn := src_p^.grn;
          blu := src_p^.blu;
          if alpha < 255 then begin    {this pixel is not fully opaque ?}
            red := (red * 255) div alpha; {restore fully opaque color value}
            red := min(red, 255);
            grn := (grn * 255) div alpha;
            grn := min(grn, 255);
            blu := (blu * 255) div alpha;
            blu := min(blu, 255);
            end;
          if d.dith.red[red].frac >= dith[yd, xd]
            then g := d.dith.red[red].high
            else g := d.dith.red[red].low;
          if d.dith.grn[grn].frac >= dith[yd, xd]
            then g := g + d.dith.grn[grn].high
            else g := g + d.dith.grn[grn].low;
          if d.dith.blu[blu].frac >= dith[yd, xd]
            then g := g + d.dith.blu[blu].high
            else g := g + d.dith.blu[blu].low;
          dst_p^ := chr(g);            {save GIF pixel value in GIF image array}
          end
        else begin                     {make this pixel transparent}
          dst_p^ := chr(d.nlut);
          end
        ;
      xd := xd + 1;                    {across this dither pattern row}
      if xd > 3 then xd := 0;          {wrap back to start of row ?}
      src_p := univ_ptr(               {advance to next source pixel}
        sys_int_adr_t(src_p) + sizeof(src_p^));
      dst_p := univ_ptr(               {advance to next destination pixel}
        sys_int_adr_t(dst_p) + sizeof(dst_p^));
      end;                             {back for next pixel in this scan line}
    yd := yd + 1;                      {to the next dither pattern row down}
    if yd > 3 then yd := 0;            {wrap back to the top row ?}
    end;                               {back for next scan line in image}
  end;
{
****************************************************
*
*   Subroutine W_CONV_GRAY (D)
*
*   Process the saved input image data at D.PIX_SRC_P to create the GIF
*   output pixel values at D.PIX_GIF_P.  The input pixel data may get
*   trashed.
*
*   This routine converts the input pixels to gray scale.  The LUT is
*   already assumed to have been loaded with a gray ramp.
}
procedure w_conv_gray (                {make GIF pixel values from saved input image}
  in out  d: data_write_t);            {our private GIF writing state block}
  internal;

var
  src_p: write_pix_p_t;                {pointer to current saved source pixel}
  dst_p: ^char;                        {pointer to current GIF desination pixel}
  npix: sys_int_machine_t;             {total number of pixels in the image}
  r: real;                             {scratch floating point value}
  i: sys_int_machine_t;                {loop counter}
  g: sys_int_machine_t;                {GIF pixel value}
  maxg: sys_int_machine_t;             {max allowed GIF pixel value for gray}

begin
  npix := d.size_x * d.size_y;         {make total number of pixels in the image}
  src_p := d.pix_src_p;                {init pointer to first source pixel}
  dst_p := d.pix_gif_p;                {init pointer to first destination pixel}
  maxg := d.nlut - 1;                  {max valid gray GIF pixel value}

  if d.transparent
    then begin                         {totally transparent LUT index exists}
      for i := 0 to npix-1 do begin    {once for each pixel in the image}
        if src_p^.alpha >= 128
          then begin                   {make the pixel opaque}
            r := (                     {make 0 to 1 gray value}
              src_p^.red * 0.299 +
              src_p^.grn * 0.587 +
              src_p^.blu * 0.114) / 255.0;
            g := trunc(r * d.nlut);    {make raw color index value}
            g := min(maxg, g);         {clip to max allowed value}
            dst_p^ := chr(g);          {save GIF pixel value in GIF image array}
            end
          else begin                   {make the pixel transparent}
            dst_p^ := chr(d.nlut);
            end
          ;
        src_p := univ_ptr(             {advance to next source pixel}
          sys_int_adr_t(src_p) + sizeof(src_p^));
        dst_p := univ_ptr(             {advance to next destination pixel}
          sys_int_adr_t(dst_p) + sizeof(dst_p^));
        end;                           {back for next pixel in image}
      end
    else begin                         {all pixels will be opaque}
      for i := 0 to npix-1 do begin    {once for each pixel in the image}
        r := (                         {make 0 to 1 gray value}
          src_p^.red * 0.299 +
          src_p^.grn * 0.587 +
          src_p^.blu * 0.114) / 255.0;
        g := trunc(r * d.nlut);        {make raw color index value}
        g := min(maxg, g);             {clip to max allowed value}
        dst_p^ := chr(g);              {save GIF pixel value in GIF image array}
        src_p := univ_ptr(             {advance to next source pixel}
          sys_int_adr_t(src_p) + sizeof(src_p^));
        dst_p := univ_ptr(             {advance to next destination pixel}
          sys_int_adr_t(dst_p) + sizeof(dst_p^));
        end;                           {back for next pixel in image}
      end
    ;
  end;
{
****************************************************
*
*   Subroutine W_SCAN1 (IMG, SCAN, STAT)
*
*   Write the scan line SCAN to the image file.
}
procedure w_scan1 (                    {write next scan line from format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan1_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  internal;

var
  di_p: img_conn2_p_t;                 {pointer to IMG library internal conn data}
  d_p: data_write_p_t;                 {pointer to our private GIF writing data}
  p: write_pix_p_t;                    {pointer to current saved input pixel}
  x: sys_int_machine_t;                {scan line pixel index}
  y: sys_int_machine_t;                {scan line Y coordinate}

begin
  sys_error_none (stat);               {init to no error occurred}
  di_p := img.data_p;                  {get pointer to IMG library internal data}
  d_p := di_p^.data_p;                 {get pointer to our private GIF writing data}
  with d_p^: d do begin                {D is abbrev for private GIF writing data}

  y := max(0, min(d.size_y - 1, img.next_y)); {get clipped Y coordinate}
  p := univ_ptr(                       {make pointer to first destination pixel}
    sys_int_adr_t(d.pix_src_p) + y * d.pix_src_dy);

  for x := 0 to d.size_x-1 do begin    {once for each pixel in the scan line}
    p^.alpha := scan[x].alpha;         {save pixel in our internal image buffer}
    p^.red := scan[x].red;
    p^.grn := scan[x].grn;
    p^.blu := scan[x].blu;
    p := univ_ptr(                     {advance to next destination pixel}
      sys_int_adr_t(p) + sizeof(p^));
    end;                               {back to copy next pixel in the scan line}

  img.next_y := y + 1;                 {update Y coordinate of next scan line}
  if y <> d.size_y-1 then return;      {we didn't just write the last scan line ?}
{
*   We just received the last scan line for this image.
*   Now convert our saved input values into the final GIF output pixel values.
}
  case d.conv of                       {what is the pixel conversion method ?}
write_conv_pick_k: begin               {pick nearest}
      w_conv_pick (d);
      end;
write_conv_4x4_k: begin                {4x4 dither pattern}
      w_conv_4x4 (d);
      end;
write_conv_gray_k: begin               {gray scale}
      w_conv_gray (d);
      end;
otherwise                              {any method we don't recognize}
    w_conv_pick (d);                   {pick nearest color for each pixel}
    end;

  write_image (d);                     {write the GIF image to the output file}
  end;                                 {done with D abbreviation}
  end;
{
****************************************************
*
*   Subroutine W_CLOSE (IMG, STAT)
*
*   Close the connection to this image file that is open for write.
}
procedure w_close (                    {close this connection}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}
  internal;

var
  di_p: img_conn2_p_t;                 {pointer to IMG library internal conn data}
  d_p: data_write_p_t;                 {pointer to our private GIF writing data}

begin
  sys_error_none (stat);               {init to no error occurred}
  di_p := img.data_p;                  {get pointer to IMG library internal data}
  d_p := di_p^.data_p;                 {get pointer to our private GIF writing data}
  with d_p^: d do begin                {D is abbrev for private GIF writing data}

  write_byte (d, 16#3B);               {write end of GIF data byte}
  write_flush (d);                     {flush the output buffer to the output file}
  file_close (d.conn);                 {close the GIF output file}
  end;                                 {done with D abbreviation}
  end;
{
****************************************************
*
*   Subroutine SET_DITH_ENTRY (N, LOW, HIGH, ILOW, IHIGH, NCOL, MULT, ENTRY)
*
*   Set the dither table entry ENTRY.  N is the 0-255 input component value
*   for this entry.  LOW and HIGH are the 0-255 closest available component
*   values to N.  LOW is always <= N, HIGH is always >= N.
*   ILOW and IHIGH are the sequential value numbers of LOW and HIGH.
*   For example, if the availble output values are 0, 128, and 255, and N is
*   139, then LOW = 128, HIGH = 255, ILOW = 1, IHIGH = 2.  NCOL is the total
*   number of available output component values.  Therefore ILOW and IHIGH
*   may never exceed NCOL-1.  MULT is the number of LUT entries for this
*   component in a row that have the same value.  MULT is always 1 for blue,
*   WRITE_N_BLU_K for green, and WRITE_N_BLU_K * WRITE_N_GRN_K for red.
}
procedure set_dith_entry (             {fill in one dither array entry for one comp}
  in      n: sys_int_machine_t;        {0-255 input component value}
  in      low, high: sys_int_machine_t; {surrounding available component values}
  in      ilow, ihigh: sys_int_machine_t; {sequential level values of LOW and HIGH}
  in      ncol: sys_int_machine_t;     {total number of output levels}
  in      mult: sys_int_machine_t;     {LUT index multiplier for this coponent}
  out     entry: dith_entry_t);        {dither table entry to fill in}
  val_param;

var
  d: sys_int_machine_t;                {delta from LOW to HIGH}

begin
  entry.close :=                       {set LUT offset for closest approximation}
    ((n * ncol) div 256) * mult;

  entry.low := ilow * mult;            {set LUT offset for low approximation}
  d := high - low;                     {size of gap from LOW to HIGH}
  if d <= 0 then begin                 {at end of range where no HIGH exists ?}
    entry.high := entry.low;
    entry.frac := 0;                   {indicate always use LOW entry}
    return;
    end;
  entry.high := ihigh * mult;          {set LUT offset for high approximation}

  entry.frac :=                        {fraction into range from LOW to HIGH}
    (dith_frac_high_k * (n - low)) div d;
  end;
{
****************************************************
*
*   Subroutine OPEN_WRITE_NEXT (IMG, X_SIZE, Y_SIZE, PARMS, STAT)
*
*   Set up for writing the next image in the same image file.  The last
*   scan line must have been written for the previous image.
}
procedure open_write_next (            {open next image for write in same image file}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      x_size, y_size: sys_int_machine_t; {size of image in pixels}
  in      parms: univ string_var_arg_t; {additional format commands}
  out     stat: sys_err_t);            {completion status code}
  val_param;

var
  di_p: img_conn2_p_t;                 {pointer to IMG library internal conn data}
  d_p: data_write_p_t;                 {pointer to GIF driver private data}
  pval: img_parms_t;                   {values from standard PARMS commands}

begin
  sys_error_none (stat);               {init to no error occurred}
  di_p := img.data_p;                  {get pointer to IMG library internal data}
  d_p := di_p^.data_p;                 {get pointer to our private GIF writing data}
  with d_p^: d do begin                {D is abbrev for private GIF writing data}

  if img.next_y <> d.size_y then begin {not just wrote last scan of old image ?}
    sys_stat_set (img_subsys_k, img_err_old_ndone_k, stat);
    return;
    end;

  if (x_size <> d.size_x) or (y_size <> d.size_y) then begin {size change ?}
    sys_stat_set (img_subsys_k, img_err_size_change_k, stat);
    return;
    end;
{
*   Process the PARMS string.  Note that we don't allow much of the configuration
*   to change from what was originally established with the first image.
*   Depending on the state, some changes are ignored, while others cause
*   hard errors.
}
  img_parms_read (parms, pval, stat);  {read PARMS and interpret standard commands}
  if sys_error(stat) then return;

  if img_parm_rate_k in pval.expl then begin {animation rate explicitly set ?}
    d.animsec := 1.0 / pval.rate;      {set new animation delay time for this image}
    end;
{
*   Update state for starting a new image.
}
  img.next_y := 0;                     {next scan line will be first in new image}
  end;                                 {done with D abbreviation}
  end;
{
****************************************************
*
*   Subroutine IMG_D_GIF_OPEN_WRITE (IMG, PARMS, STAT)
*
*   Open a GIF file for write.  The entire IMG data structure has already been
*   filled in, and the internal IMG library connection block allocated and
*   partially filled in.  PARMS is a string of optional parameters that the
*   application may pass.  It may contain a list of keywords followed by
*   parameters.  The keywords unique to this driver are:
*
*   -DITH <dithering method>
*
*     This command controls how the incoming 8 bit per color per pixel true
*     color values are to be converted to the GIF 8 bit pseudo color values.
*     Choices are:
*
*       NONE  -  Picks closest color from 240 (6 red, 8 green, 5 blue).
*         When transparency is enabled (ALPHA with value > 0), then 241
*         possible values are written to the output file.  These are the
*         240 color plus one value for transparent.
*
*       4x4  -  4 x 4 regular dither pattern.
*
*     The default is 4x4.
}
procedure img_d_gif_open_write (       {open GIF file for write}
  in out  img: img_conn_t;             {handle to new image file connection}
  in      parms: univ string_var_arg_t; {parameter string passed to driver}
  out     stat: sys_err_t);            {completion status code}

var
  di_p: img_conn2_p_t;                 {pointer to IMG library internal conn data}
  d_p: data_write_p_t;                 {pointer to GIF driver private data}

  i, j: sys_int_machine_t;             {scratch integer}
  n: sys_int_machine_t;
  m: sys_int_machine_t;                {LUT index mult factor}
  red, grn, blu: sys_int_machine_t;    {scratch color component values}
  ir, ig, ib: sys_int_machine_t;       {loop counters}

  p: string_index_t;                   {PARMS parse index}
  opt: string_var32_t;                 {PARMS option name}
  parm: string_var32_t;                {parameter for option name in OPT}
  pick: sys_int_machine_t;             {number of token picked from list}
  pval: img_parms_t;                   {values from standard PARMS commands}

label
  next_opt, bad_parm, done_opts;

begin
  opt.max := sizeof(opt.str);          {init local var strings}
  parm.max := sizeof(parm.str);
{
*   Fill in the internal IMG library data block for this connection.
}
  di_p := img.data_p;                  {get pointer to internal IMG library data}
  util_mem_grab (                      {allocate private GIF file write data block}
    sizeof(d_p^),                      {amount of memory to allocate}
    di_p^.mem_p^,                      {parent memory context}
    false,                             {we won't individually deallocate this}
    d_p);                              {returned pointer to new memory}

  di_p^.data_p := d_p;                 {save pointer to our private data block}
  di_p^.write_scan1 := addr(w_scan1);  {set pointer to our write scan line routine}
  di_p^.close := addr(w_close);        {set pointer to our close routine}
  di_p^.next_write := addr(open_write_next); {pnt to routine to create next image}
{
*   Initialize some of the state in our private GIF file writing data
*   block at D_P^.
}
  with d_p^: d do begin                {D is abbreviation for our private data block}

  d.img_conn_p := addr(img);
  d.img_conn2_p := di_p;
  d.animsec := 0.1;
  d.transparent := false;
  d.anim := false;
  d.loop := false;
  d.conv := write_conv_4x4_k;
  d.size_x := img.x_size;
  d.size_y := img.y_size;
  d.pix_nbits := 8;
  d.lzw_free_p := nil;
  d.lzw_last_p := nil;
  d.lzw_root.next_p := nil;
  d.code_n := 0;
  d.code_buf := 0;
  d.data_n := 0;
  d.buf_n := 0;
{
*   Process PARMS string.
}
  img_parms_read (parms, pval, stat);  {read PARMS and interpret standard commands}
  p := 1;                              {init index to unprocessed commands}

next_opt:                              {back here each new command from format str}
  string_token (pval.rem, p, opt, stat); {extract next unprocessed PARMS token}
  if string_eos(stat) then goto done_opts; {reached end of PARMS ?}
  string_upcase (opt);                 {make upper case for token matching}
  string_tkpick80 (opt,                {pick token from list of legal keywords}
    '-DITH',
    pick);
  case pick of                         {which command is it ?}
{
*   DITH <method>
}
1: begin
  string_token (pval.rem, p, parm, stat); {get parameter string}
  if sys_error(stat) then goto bad_parm;
  string_upcase (parm);                {make upper case for keyword matching}
  string_tkpick80 (parm,
    'NONE 4X4',
    pick);
  case pick of
1:  d.conv := write_conv_pick_k;       {pick nearest of 240 colors}
2:  d.conv := write_conv_4x4_k;        {4x4 dither pattern of 240 colors}
otherwise
    goto bad_parm;                     {unrecognized method keyword}
    end;
  end;
{
*   Unrecognized command name.
}
otherwise
    img_parms_skip (pval.rem, p);      {skip over parameters to the urecognized cmd}
    goto next_opt;                     {back for next command}
    end;                               {done with option name cases}

  if not sys_error(stat) then goto next_opt; {no error, back for next keyword ?}
bad_parm:                              {jump here on bad parameter to particular OPT}
  sys_stat_set (img_subsys_k, img_err_bad_fmt_parm_k, stat);
  sys_stat_parm_vstr (parm, stat);
  sys_stat_parm_vstr (opt, stat);
  sys_stat_parm_vstr (img.tnam, stat);
  return;
done_opts:                             {jump here when processed whole PARMS string}

  if pval.bits_alpha > 0               {check if alpha enabled}
    then d.transparent := true
    else d.transparent := false;

  if pval.gray then begin              {pixel values will all be gray ?}
    d.conv := write_conv_gray_k;
    end;

  d.anim := pval.anim;                 {set animation flag}
  d.loop := pval.anim and pval.loop;   {set animation loop flag}
  d.animsec := 1.0 / pval.rate;        {set animation time per frame}
{
*   Done processing the PARMS string.
*
*   Open the output file.
}
  file_open_write_bin (                {open GIF output file for write}
    img.tnam,                          {file name}
    '.gif',                            {mandatory file name suffix}
    d.conn,                            {returned handle to new file connection}
    stat);
  if sys_error(stat) then return;
{
*   Allocate memory for the raw and GIF pixel buffers.
}
  d.pix_src_dy :=                      {memory stride for each source scan line down}
    sizeof(write_pix_t) * d.size_x;
  util_mem_grab (                      {allocate mem for saved input pixels}
    d.pix_src_dy * d.size_y,           {amount of memory to allocate}
    di_p^.mem_p^,                      {parent memory context}
    true,                              {we will individually deallocate this}
    d.pix_src_p);                      {returned pointer to the new memory}

  d.pix_gif_dy :=                      {memory stride for each GIF scan line down}
    sizeof(char) * d.size_x;
  util_mem_grab (                      {allocate mem for GIF output pixels}
    d.pix_gif_dy * d.size_y,           {amount of memory to allocate}
    di_p^.mem_p^,                      {parent memory context}
    true,                              {we will individually deallocate this}
    d.pix_gif_p);                      {returned pointer to the new memory}
{
*   Fill in the LUT.  This table translates GIF file pseudo color pixel values
*   to their true RGBA values.
}
  d.nlut := 0;                         {init to no color table values set}
  case d.conv of                       {what is color conversion method ?}

write_conv_pick_k,                     {pick nearest of 240 colors}
write_conv_4x4_k: begin                {dither from 240 colors}
      i := 0;                          {init starting LUT index to write}
      for ir := 0 to write_n_red_k-1 do begin
        red := min(255, (ir * 256) div (write_n_red_k - 1));
        for ig := 0 to write_n_grn_k-1 do begin
          grn := min(255, (ig * 256) div (write_n_grn_k - 1));
          for ib := 0 to write_n_blu_k-1 do begin
            blu := min(255, (ib * 256) div (write_n_blu_k - 1));
            d.lut[i].alpha := 255;     {fill in this LUT entry}
            d.lut[i].red := red;
            d.lut[i].grn := grn;
            d.lut[i].blu := blu;
            i := i + 1;                {make index of next LUT entry}
            end;
          end;
        end;
      d.nlut := i;                     {save number of entries in LUT}
      end;

write_conv_gray_k: begin               {convert to gray scale}
      if d.transparent                 {set max LUT index available for gray ramp}
        then j := 254
        else j := 255;
      for i := 0 to j do begin         {once for each LUT entry in gray ramp}
        red := trunc(256.0 * i / j);   {make raw gray value here}
        red := min(255, red);          {clip to max range}
        d.lut[i].alpha := 255;         {fill in this LUT entry}
        d.lut[i].red := red;
        d.lut[i].grn := red;
        d.lut[i].blu := red;
        end;                           {back to fill in next LUT entry}
      d.nlut := j + 1;                 {indicate number of valid LUT entries}
      end;

    end;                               {end of color conversion type cases}
{
*   Fill in the DITH table.  This table is used to generate the final pseudo
*   color values from the 0-255 component true color input values.  The
*   DITH table may not be used depending on the conversion mode.
}
  i := 0;                              {init index for LOW value}
  j := 1;                              {init index for HIGH value}
  for n := 0 to 255 do begin           {once for each possible input value}
    if n >= d.lut[j].blu then begin    {switch to next interval up ?}
      i := j;                          {old HIGH becomes new LOW}
      j := j + 1;                      {advance HIGH to end of next interval}
      j := min(j, write_n_blu_k - 1);  {clip to max available level}
      end;
    set_dith_entry (                   {fill in this DITH entry for this component}
      n,                               {0-255 component value here}
      d.lut[i].blu,                    {low component value}
      d.lut[j].blu,                    {high component value}
      i, j,                            {sequential LOW and HIGH level numbers}
      write_n_blu_k,                   {total number of component output levels}
      1,                               {LUT index multiplier}
      d.dith.blu[n]);                  {DITH table component entry to fill in}
    end;                               {back for next 0-255 input value}

  i := 0;                              {init index for LOW value}
  j := 1;                              {init index for HIGH value}
  m := write_n_blu_k;                  {LUT index mult factor for this component}
  for n := 0 to 255 do begin           {once for each possible input value}
    if n >= d.lut[j*m].grn then begin  {switch to next interval up ?}
      i := j;                          {old HIGH becomes new LOW}
      j := j + 1;                      {advance HIGH to end of next interval}
      j := min(j, write_n_grn_k - 1);  {clip to max available level}
      end;
    set_dith_entry (                   {fill in this DITH entry for this component}
      n,                               {0-255 component value here}
      d.lut[i * m].grn,                {low component value}
      d.lut[j * m].grn,                {high component value}
      i, j,                            {sequential LOW and HIGH level numbers}
      write_n_grn_k,                   {total number of component output levels}
      m,                               {LUT index multiplier}
      d.dith.grn[n]);                  {DITH table component entry to fill in}
    end;                               {back for next 0-255 input value}

  i := 0;                              {init index for LOW value}
  j := 1;                              {init index for HIGH value}
  m := write_n_blu_k * write_n_grn_k;  {LUT index mult factor for this component}
  for n := 0 to 255 do begin           {once for each possible input value}
    if n >= d.lut[j*m].red then begin  {switch to next interval up ?}
      i := j;                          {old HIGH becomes new LOW}
      j := j + 1;                      {advance HIGH to end of next interval}
      j := min(j, write_n_red_k - 1);  {clip to max available level}
      end;
    set_dith_entry (                   {fill in this DITH entry for this component}
      n,                               {0-255 component value here}
      d.lut[i * m].red,                {low component value}
      d.lut[j * m].red,                {high component value}
      i, j,                            {sequential LOW and HIGH level numbers}
      write_n_red_k,                   {total number of component output levels}
      m,                               {LUT index multiplier}
      d.dith.red[n]);                  {DITH table component entry to fill in}
    end;                               {back for next 0-255 input value}
{
*   Write GIF header.
}
  write_byte (d, ord('G'));            {"GIF" signature bytes}
  write_byte (d, ord('I'));
  write_byte (d, ord('F'));
  write_byte (d, ord('8'));            {requires reader min version "89a"}
  write_byte (d, ord('9'));
  write_byte (d, ord('a'));
{
*   Write GIF logical screen descriptor.
}
  write_i16 (d, d.size_x);             {width}
  write_i16 (d, d.size_y);             {height}
  i :=                                 {make byte with packed fields}
    lshft(1, 7) !                      {global color table follows logcal scr desc}
    lshft(7, 4) !                      {source color table size (8 bits)}
    lshft(0, 3) !                      {color table is not sorted by importance}
    7;                                 {Log2(LUT entries) - 1}
  write_byte (d, i);                   {write packed fields byte}
  write_byte (d, 0);                   {pixel value to assume for background color}
  i :=                                 {pixel aspect ratio in 1/64 increments}
    round(64.0 * img.aspect * img.y_size / img.x_size) - 15;
  i := min(255, max(1, i));            {adjust offset and clip to 1-255 legal range}
  write_byte (d, i);                   {write pixel aspect ratio byte}
{
*   Write the GIF global color table.
}
  for i := 0 to 255 do begin           {once for each LUT entry to write}
    if i <= d.nlut
      then begin                       {this index is still within our LUT}
        write_byte (d, d.lut[i].red);  {write the color table entry from out LUT}
        write_byte (d, d.lut[i].grn);
        write_byte (d, d.lut[i].blu);
        end
      else begin                       {we don't use this pseudo color}
        write_byte (d, 0);             {just write all black}
        write_byte (d, 0);
        write_byte (d, 0);
        end
      ;
    end;                               {back to write next global color table entry}
{
*   Write the Netscape extension block for looping thru animations if this
*   GIF file is intended to hold an animation and it's supposed to loop.
}
  if d.anim and pval.loop then begin   {supposed to be animation loop ?}
    write_byte (d, 33);                {GIF extension code}
    write_byte (d, 255);               {application extension label}
    write_byte (d, 11);                {number of data bytes in extension block}
    write_byte (d, ord('N'));          {the 11 data bytes, "NETSCAPE2.0"}
    write_byte (d, ord('E'));
    write_byte (d, ord('T'));
    write_byte (d, ord('S'));
    write_byte (d, ord('C'));
    write_byte (d, ord('A'));
    write_byte (d, ord('P'));
    write_byte (d, ord('E'));
    write_byte (d, ord('2'));
    write_byte (d, ord('.'));
    write_byte (d, ord('0'));

    write_byte (d, 3);                 {number of data bytes in this data sub-block}
    write_byte (d, 1);                 {required, can't find spec what this means}
    write_i16 (d, 0);                  {write iteration count, 0 = infinite}

    write_byte (d, 0);                 {data sub-blocks terminator}
    end;
{
*   Fill in IMG, the app-visible image connection state block.
}
  if d.transparent
    then img.bits_alpha := 1
    else img.bits_alpha := 0;
  img.bits_red := 8;
  img.bits_grn := 8;
  img.bits_blu := 8;
  img.bits_max := 8;
  end;                                 {done with D abbreviation}
  end;
{
*******************************************************************
*******************************************************************
*
*   This section contains information that is unique to reading
*   GIF files.
}
const
  read_buf_size_k = 2048;              {bytes read at one time from the intput file}
  read_buf_last_k =                    {last valid read buffer index}
    read_buf_size_k - 1;

type
  lut_t = array[0..255] of img_pixel1_t; {translate GIF pixel values to true color}

  read_code_t = record                 {one entry in LZW codes table}
    last: sys_int_machine_t;           {last value in string for this code}
    prev: sys_int_machine_t;           {previous code in string,
                                        -1 = terminal, -2 = clear, -3 = end}
    end;

  read_code_buf_t =                    {buffer to hold expansion of any LZW code}
    array[0..4096] of char;

  scans_t =                            {pointers to the start of each scan line}
    array[0..0] of ^char;
  scans_p_t = ^scans_t;

  data_read_t = record                 {private connection data for reading GIF file}
    img_p: img_conn_p_t;               {points to app image connection handle}
    img2_p: img_conn2_p_t;             {points to internal IMG lib data for this img}
    size_x, size_y: sys_int_machine_t; {image size in pixels}
    pasp: real;                        {pixel aspect ratio}
    interlace: boolean;                {TRUE if GIF scan lines interlaced}
    transparent: boolean;              {TRUE if transparent color present}
    img_set: boolean;                  {TRUE if image at PIX_FIRST_P already read in}
    pix_first_p: ^char;                {pointer to first GIF pixel value}
    scans_p: scans_p_t;                {pnt to array of GIF pixel scan line pointers}
    pix_p: ^char;                      {pointer to current GIF pixel value}
    pix_left: sys_int_machine_t;       {number of GIF pixels left to process}
    lut_global: lut_t;                 {color table for whole file}
    lut: lut_t;                        {actual color table for this image}
    lut_n: sys_int_machine_t;          {number of LUT entries in GIF file}
    code_n: sys_int_machine_t;         {number of bits left unread in CODE_BUF}
    code_buf: sys_int_machine_t;       {unread bits from last LZW byte in LSB}
    code_nbits_val: sys_int_machine_t; {LZW code size for a raw terminal value}
    code_nbits: sys_int_machine_t;     {current LZW code size in bits}
    code_left_size: sys_int_machine_t; {number of codes left to read at curr size}
    lzw_old: sys_int_machine_t;        {last LZW code read, -1 = none}
    lzw_next: sys_int_machine_t;       {index for next table entry to make}
    lzw_clear: sys_int_machine_t;      {LZW clear code value}
    lzw_end: sys_int_machine_t;        {LZW end of information code value}
    data_n: sys_int_machine_t;         {number of data bytes remaining in sub-block}
    buf_n: sys_int_machine_t;          {number of bytes left to read from BUF}
    buf_ind: sys_int_machine_t;        {BUF index to read next byte from}
    buf_eof: boolean;                  {TRUE on input file end encountered}
    conn: file_conn_t;                 {handle to input file connection}
    stat: sys_err_t;                   {contains status when ERR returned TRUE}
    buf: array[0..read_buf_last_k] of char; {buffer for reading from GIF file}
    codes:                             {data for each possible LZW code}
      array[0..4095] of read_code_t;
    end;
  data_read_p_t = ^data_read_t;
{
****************************************************
*
*   Function READ_BYTE (D, ERR)
*
*   Read the next byte from the input file.  The input file data is actually
*   buffered, but this is transparent to the upper levels.
}
function read_byte (                   {read next input file byte}
  in out  d: data_read_t;              {our private GIF reading state block}
  out     err: boolean)                {TRUE with D.STAT set on error}
  :sys_int_machine_t;                  {0-255 byte value}
  internal;

var
  olen: sys_int_adr_t;                 {number of bytes actually read}

label
  return_eof;

begin
  if d.buf_n = 0 then begin            {input buffer is empty ?}
    if d.buf_eof then begin            {already hit end of input file ?}
return_eof:                            {jump here to return with end of file status}
      sys_stat_set (file_subsys_k, file_stat_eof_k, d.stat);
      err := true;
      read_byte := 0;
      return;                          {return with END OF FILE status}
      end;
    file_read_bin (                    {try to read next buffer full from file}
      d.conn,                          {handle to file connection}
      sizeof(d.buf),                   {max data to read}
      d.buf,                           {input buffer}
      olen,                            {amount of data actually read}
      d.stat);
    if sys_error(d.stat) then begin    {not just a normal read ?}
      if file_eof(d.stat) or file_eof_partial(d.stat) {hit end of file ?}
        then begin                     {hit end of file}
          d.buf_eof := true;           {indicate nothing more to read from file}
          file_close (d.conn);         {we don't need the file anymore}
          if olen = 0 then goto return_eof; {we didn't get any data before file end ?}
          end
        else begin                     {a real error ocurred}
          err := true;                 {indicate STAT set}
          read_byte := 0;
          return;                      {return with error}
          end
        ;
      end;                             {done handling abnormal condition from read}
    d.buf_n := olen;                   {set number of bytes now in buffer}
    d.buf_ind := 0;                    {init index for next byte to read from buf}
    end;                               {done reading next chunk into buffer}

  read_byte := ord(d.buf[d.buf_ind]);  {fetch this byte from the input buffer}
  d.buf_n := d.buf_n - 1;              {one less byte left in input buffer}
  d.buf_ind := d.buf_ind + 1;          {update index to read next byte from}
  err := false;                        {indicate STAT not set}
  end;
{
****************************************************
*
*   Function READ_I16 (D, ERR)
*
*   Read a 16 bit integer from the input data stream.
}
function read_i16 (                    {read 16 bit integer from input data stream}
  in out  d: data_read_t;              {our private GIF reading state block}
  out     err: boolean)                {TRUE with D.STAT set on error}
  :sys_int_machine_t;                  {0-65535 integer value}
  internal;

var
  i: sys_int_machine_t;

begin
  read_i16 := 0;                       {default return value on error}
  i := read_byte(d, err);              {read low byte of 16 bit integer}
  if err then return;
  read_i16 := i ! lshft(read_byte(d, err), 8); {read high byte of 16 bit integer}
  end;
{
****************************************************
*
*   Function READ_BYTE_DATA (D, ERR)
*
*   Read the next data byte encapsulated in GIF data sub-blocks.  This routine
*   removes the sub-block wrappers and returns just the stream of data bytes.
*   STAT is set to end of file with ERR TRUE when the end of the data stream
*   is encountered.
}
function read_byte_data (              {read next data byte from sub-block stream}
  in out  d: data_read_t;              {our private GIF reading state block}
  out     err: boolean)                {TRUE with D.STAT set on error}
  :sys_int_machine_t;                  {0-255 byte value}
  internal;

begin
  read_byte_data := 0;                 {default return value on error}

  if d.data_n <= 0 then begin          {at start of a new sub-block ?}
    d.data_n := read_byte(d, err);     {get number of bytes in this new sub-block}
    if err then return;
    if d.data_n = 0 then begin         {hit end of sub-blocks stream ?}
      sys_stat_set (file_subsys_k, file_stat_eof_k, d.stat);
      err := true;
      return;                          {return with END OF FILE status}
      end;
    end;

  read_byte_data := read_byte(d, err); {get next data byte}
  d.data_n := d.data_n - 1;            {one less byte left in this sub-block}
  end;
{
****************************************************
*
*   Subroutine READ_LUT (D, LUT, ERR)
*
*   Read the LUT information from the GIF file.  The LUT translates the 0-255
*   GIF pixel values to a full true color format 1 pixel value.
}
procedure read_lut (                   {read LUT from GIF file}
  in out  d: data_read_t;              {our private GIF reading state block}
  out     lut: lut_t;                  {LUT to fill in}
  out     err: boolean);               {TRUE with D.STAT set on error}
  internal;

var
  i: sys_int_machine_t;                {current LUT index}

begin
  for i := 0 to d.lut_n-1 do begin     {once for each entry to read from GIF file}
    lut[i].alpha := 255;
    lut[i].red := read_byte (d, err);
    if err then return;
    lut[i].grn := read_byte (d, err);
    if err then return;
    lut[i].blu := read_byte (d, err);
    if err then return;
    end;

  for i := d.lut_n to 255 do begin     {once for each remaining unused LUT entry}
    lut[i].alpha := 255;
    lut[i].red := 0;
    lut[i].grn := 0;
    lut[i].blu := 0;
    end;
  end;
{
****************************************************
*
*   Subroutine READ_CLEAR (D)
*
*   Perform the LZW clear operation.  This resets the LZW decoding state
*   to no accumulated strings.
}
procedure read_clear (                 {reset LZW decoder state}
  in out  d: data_read_t);             {our private GIF reading state block}
  internal;

var
  i: sys_int_machine_t;                {loop counter}

begin
  d.lzw_old := -1;                     {init to no "old" code exists}
  d.lzw_next := d.lzw_end + 1;         {index for next table entry to create}

  d.code_nbits := d.code_nbits_val + 1; {reset LZW code size to starting value}
  d.code_left_size :=                  {number of codes left at this code size}
    lshft(1, d.code_nbits) - d.lzw_next + 1;

  for i := d.lzw_next to 4095 do begin {once for each dynamic code in table}
    d.codes[i].last := 0;              {reset all as terminal codes with value 0}
    d.codes[i].prev := -1;
    end;
  end;
{
****************************************************
*
*   Subroutine READ_LZW_INIT (D, ERR)
*
*   Initialize the LZW decoder state for this image.  This routine reads the
*   first byte of the raster image data stream.  This byte indicates the
*   number of bits needed to represent all the terminal values.  The initial
*   LZW code size is therefore this number plus 1.
}
procedure read_lzw_init (              {init LZW decoder state for this image}
  in out  d: data_read_t;              {our private GIF reading state block}
  out     err: boolean);               {TRUE with D.STAT set on error}
  internal;

var
  i: sys_int_machine_t;                {scratch integer and loop counter}

begin
  d.code_nbits_val := read_byte(d, err); {get the number of bits for actual pix vals}
  if err then return;

  d.lzw_clear := lshft(1, d.code_nbits_val); {make LZW clear code value}
  d.lzw_end := d.lzw_clear + 1;        {make LZW end of data code value}

  for i := 0 to d.lzw_clear-1 do begin {once for all the terminal data value codes}
    d.codes[i].last := i;              {data value is same as code value}
    d.codes[i].prev := -1;             {indicate this is a terminal code}
    end;

  d.codes[d.lzw_clear].last := 0;      {set table entry for clear code}
  d.codes[d.lzw_clear].prev := -2;
  d.codes[d.lzw_end].last := 0;        {set table entry for end code}
  d.codes[d.lzw_end].prev := -3;

  d.data_n := 0;                       {we are now at start of new data sub block}
  d.code_n := 0;                       {there are no unread bits in CODE_BUF}
  d.code_buf := 0;

  read_clear (d);                      {do CLEAR to protect against bad input data}
  end;
{
****************************************************
*
*   Function READ_LZW_CODE (D, ERR)
*
*   Return the next raw LZW code from the input stream.  This routine unpacks
*   the code from the data stream and returns just the code value.  It returns
*   end of file status when the end of the LZW code stream is reached.
}
function read_lzw_code (               {return next raw LZW code}
  in out  d: data_read_t;              {our private GIF reading state block}
  out     err: boolean)                {TRUE with D.STAT set on error}
  :sys_int_machine_t;                  {LZW code value}
  internal;

var
  code: sys_int_machine_t;             {assembled code}
  bits_code: sys_int_machine_t;        {number of bits currently in CODE}
  bits_left: sys_int_machine_t;        {number of bits left to fetch into CODE}
  bt: sys_int_machine_t;               {number of bits adding to CODE this iteration}
  mask: sys_int_machine_t;             {for masking in bits from bit buffer}

label
  loop;

begin
  read_lzw_code := 0;                  {default return value for error}
  err := false;                        {init to no error occurred}

loop:                                  {back here after reading clear code}
  code := 0;                           {clear all accumulated code bits}
  bits_code := 0;                      {init to CODE is empty}
  bits_left := d.code_nbits;           {init number of bits left to stuff into CODE}

  while bits_left > 0 do begin         {keep reading more until CODE all filled}
    if d.code_n <= 0 then begin        {no bits left in bit buffer ?}
      d.code_buf := read_byte_data(d, err); {read next whole byte}
      if err then return;
      d.code_n := 8;                   {there are now 8 bits available in buffer}
      end;
    bt := min(d.code_n, bits_left);    {number of bits to read from buffer this time}
    bits_left := bits_left - bt;       {update number of bits to go after this time}
    mask := ~lshft(~0, bt);            {mask for bits to read from bit buffer}
    code :=                            {read bits from buffer and append to CODE}
      code ! lshft(d.code_buf & mask, bits_code);
    bits_code := bits_code + bt;       {update number of bits now in CODE}
    d.code_buf := rshft(d.code_buf, bt); {move next bit buffer bits into LSB}
    d.code_n := d.code_n - bt;         {update number of unread bits in bit buffer}
    end;                               {back to get next chunk of bits into CODE}
{
*   CODE contains the complete assembled LZW code.
}
  if code = d.lzw_clear then begin     {this was a clear code ?}
    read_clear (d);                    {reset the LZW decompressor}
    goto loop;                         {back to get the next real code}
    end;

  if code = d.lzw_end then begin       {hit end of LZW codes stream ?}
    sys_stat_set (file_subsys_k, file_stat_eof_k, d.stat); {indicate END OF FILE}
    err := true;
    return;                            {return with END OF FILE status}
    end;

  read_lzw_code := code;               {pass back LZW code}
{
*   Make sure the code size is up to date for the next time.
}
  d.code_left_size := d.code_left_size - 1; {we used up one more code at this size}
  if d.code_left_size <= 0 then begin  {we just read last code at this size ?}
    d.code_nbits := min(d.code_nbits + 1, 12); {inc code size if not already max}
    d.code_left_size := lshft(1, d.code_nbits - 1); {number of codes at new size}
    end;
  end;
{
****************************************************
*
*   Subroutine GET_CODE_STRING (D, CODE, STR, IND)
*
*   Return the expansion of the LZW code CODE in STR.  The values in STR
*   will be stored in reverse order.  IND is the STR index of the last
*   value stored in STR, which is also the first value of the string.
}
procedure get_code_string (            {get string from LZW code}
  in out  d: data_read_t;              {our private GIF reading state block}
  in      code: sys_int_machine_t;     {LZW code to expand}
  out     str: read_code_buf_t;        {returned buffer with reverse string}
  out     ind: sys_int_machine_t);     {last valid index in STR}
  val_param; internal;

var
  cd: sys_int_machine_t;               {scratch LZW code value}

begin
  ind := 0;                            {init where to put the first string value}
  cd := code;                          {init current LZW code}

  repeat                               {back here each additional value in string}
    str[ind] := chr(d.codes[cd].last); {add additional value to start of string}
    ind := ind + 1;                    {make index where to store next string value}
    cd := d.codes[cd].prev;            {get code for remaining string start}
    until cd < 0;                      {back until hit terminal code}

  ind := ind - 1;                      {make index of first value in the string}
  end;
{
****************************************************
*
*   Subroutine STRING_TO_PIX (D, STR, IND)
*
*   Write the string of pixel values in STR to the GIF pixel array.  IND
*   is the last valid STR index.  The values in STR are stored backwards,
*   so the first pixel value is at STR[IND], and the last at STR[0].
}
procedure string_to_pix (              {write values string to GIF pixel array}
  in out  d: data_read_t;              {our private GIF reading state block}
  in      str: read_code_buf_t;        {string of pixel values}
  in      ind: sys_int_machine_t);     {last valid STR index}
  val_param; internal;

var
  i: sys_int_machine_t;                {loop counter}
  ilast: sys_int_machine_t;            {index of last string char to read}

begin
  ilast := min(ind + 1, d.pix_left);   {make number of pixels to write}
  ilast := ind - ilast + 1;            {STR index of last string value to use}

  for i := ind downto ilast do begin   {once for each pixel to write}
    d.pix_p^ := str[i];                {write this string value to current pixel}
    d.pix_p := univ_ptr(               {point to next pixel in GIF image array}
      sys_int_adr_t(d.pix_p) + sizeof(d.pix_p^));
    d.pix_left := d.pix_left - 1;      {one less pixel left to write in image}
    end;                               {back to copy next pixel from string}
  end;
{
****************************************************
*
*   Subroutine READ_IMAGE (D, ERR)
*
*   Read the LZW image data and save the resulting GIF pixel values in the
*   pixel array pointed to by D.PIX_P.  The "LZW code size" byte is assumed
*   to be the next byte returned by READ_BYTE.
}
procedure read_image (                 {read LZW stream and fill in GIF pixel array}
  in out  d: data_read_t;              {our private GIF reading state block}
  out     err: boolean);               {TRUE with D.STAT set on error}
  internal;

var
  str: read_code_buf_t;                {expansion of an LZW code}
  ind: sys_int_machine_t;              {index into STR}
  code: sys_int_machine_t;             {current LZW code}
  old_first: sys_int_machine_t;        {first value from string of old code}
  p: ^char;                            {scratch pixel pointer}

begin
  read_lzw_init (d, err);              {read code size and init LZW state}
  if err then return;

  d.pix_p := d.pix_first_p;            {init pointer to first GIF pixel to write}
  d.pix_left := d.size_x * d.size_y;   {init number of GIF pixels left to write}

  while d.pix_left > 0 do begin        {keep looping until filled in all pixels}
    code := read_lzw_code(d, err);     {read next LZW code from input stream}
    if err then return;
{
*   Handle case where this is the first code since the last clear.
}
    if d.lzw_old < 0 then begin        {no previous code exists ?}
      get_code_string (                {get expansion of this code}
        d, code, str, ind);
      string_to_pix (d, str, ind);     {write code expansion to image}
      d.lzw_old := code;               {current code becomes new old code}
      old_first := ord(str[ind]);      {save first value in epansion of old code}
      next;                            {back to fill in more pixels}
      end;
{
*   Handle case where this code is not yet in the table.
}
    if code >= d.lzw_next then begin   {no table entry exists for this code ?}
      d.codes[d.lzw_next].prev := d.lzw_old; {starts with string from old code}
      d.codes[d.lzw_next].last := old_first; {ends in first char from old code str}
      get_code_string (                {get expansion of new table entry}
        d, d.lzw_next, str, ind);
      string_to_pix (d, str, ind);     {write expansion to image}
      d.lzw_next := d.lzw_next + 1;    {update table entry to make next time}
      d.lzw_old := code;               {current code becomes new old code}
      old_first := ord(str[ind]);      {save first value in epansion of old code}
      next;                            {back to fill in more pixels}
      end;
{
*   This code is already in the table.
}
    get_code_string (                  {get expansion of this code}
      d, code, str, ind);
    string_to_pix (d, str, ind);       {write code expansion to image}
    if d.lzw_next <= 4095 then begin   {we have room for a new table entry ?}
      d.codes[d.lzw_next].prev := d.lzw_old; {new entry start with str from old code}
      d.codes[d.lzw_next].last := ord(str[ind]); {set last char for new table entry}
      d.lzw_next := d.lzw_next + 1;    {update table entry to make next time}
      end;
    d.lzw_old := code;                 {current code becomes new old code}
    old_first := ord(str[ind]);        {save first value in expansion of old code}
    end;                               {back to fill in more GIF image pixels}
{
*   All the GIF pixels have been written.
}
  repeat                               {keep reading until end of information code}
    discard( read_lzw_code(d, err) );
    until err;
  if not file_eof(d.stat) then return; {encountered a hard error ?}

  repeat                               {keep reading until end of data sub blocks}
    discard( read_byte_data(d, err) );
    until err;
  if not file_eof(d.stat) then return; {encountered a hard error ?}

  err := false;                        {all went as expected}
{
*   Set the SCANS_P^ array.  This array contains one entry for each scan
*   line in the GIF pixels array.  The entry is a pointer to the first
*   pixel in that scan line.
}
  if d.interlace
    then begin                         {scan lines are stored interlaced}
      p := d.pix_first_p;              {init pointer to first scan line in array}
      ind := 0;
      while ind < d.size_y do begin
        d.scans_p^[ind] := p;          {set entry for this scan line}
        p := univ_ptr(                 {point to start of next line in array}
          sys_int_adr_t(p) + d.size_x * sizeof(p^));
        ind := ind + 8;                {make number of the next scan line}
        end;
      ind := 4;
      while ind < d.size_y do begin
        d.scans_p^[ind] := p;          {set entry for this scan line}
        p := univ_ptr(                 {point to start of next line in array}
          sys_int_adr_t(p) + d.size_x * sizeof(p^));
        ind := ind + 8;                {make number of the next scan line}
        end;
      ind := 2;
      while ind < d.size_y do begin
        d.scans_p^[ind] := p;          {set entry for this scan line}
        p := univ_ptr(                 {point to start of next line in array}
          sys_int_adr_t(p) + d.size_x * sizeof(p^));
        ind := ind + 4;                {make number of the next scan line}
        end;
      ind := 1;
      while ind < d.size_y do begin
        d.scans_p^[ind] := p;          {set entry for this scan line}
        p := univ_ptr(                 {point to start of next line in array}
          sys_int_adr_t(p) + d.size_x * sizeof(p^));
        ind := ind + 2;                {make number of the next scan line}
        end;
      end                              {done handling interlaced case}
    else begin                         {scan lines stored in top to bottom order}
      p := d.pix_first_p;              {init pointer to first scan line in array}
      for ind := 0 to d.size_y-1 do begin {down the scan lines}
        d.scans_p^[ind] := p;          {set entry for this scan line}
        p := univ_ptr(                 {point to start of next line in array}
          sys_int_adr_t(p) + d.size_x * sizeof(p^));
        end;                           {back to fill in next scan line pointer}
      end
    ;
  end;
{
****************************************************
*
*   Subroutine READ_ITEMS (D, ERR)
*
*   Read the GIF input stream until the next image is read in and stored
*   in the array pointed to by D.PIX_P.
*
*   This routine returns normally in two ways.  When no error condition is
*   set, then the next image has been found and is all set to be read in.
*   END OF FILE error status is used to indicate no image was found before
*   the end of the GIF file data stream.
*
*   This routine also returns on any hard error with error status set
*   accordingly.
}
procedure read_items (                 {read GIF file items to end of next image}
  in out  d: data_read_t;              {our private GIF reading state block}
  out     err: boolean);               {TRUE with D.STAT set on error}
  internal;

var
  id_block: sys_int_machine_t;         {GIF item block ID}
  id_ext: sys_int_machine_t;           {GIF extension ID within extension block}
  i: sys_int_machine_t;                {scratch integer and loop counter}
  transparent: sys_int_machine_t;      {LUT index of transparent color}
  sz: sys_int_adr_t;                   {memory size}
  lut_present: boolean;                {TRUE if local color table is present}

label
  loop_item;

begin
  d.transparent := false;              {init to next image has no transparency}

loop_item:                             {back here to read each new item}
  id_block := read_byte (d, err);      {get ID byte of next block}
  if err then return;
  case id_block of                     {what kind of block is this ?}
{
**************
*
*   Image Descriptor.
}
block_image_k: begin
  discard( read_i16(d, err) );         {ignore left position within virtual screen}
  if err then return;
  discard( read_i16(d, err) );         {ignore top position within virtual screen}
  if err then return;

  d.size_x := read_i16(d, err);        {get image width in pixels}
  d.size_y := read_i16(d, err);        {get image height in pixels}

  i := read_byte(d, err);              {read packed fields byte}
  if err then return;
  lut_present := (i & 16#80) <> 0;     {TRUE if local color table to follow}
  d.interlace := (i & 16#40) <> 0;     {TRUE if scan lines use GIF interlacing}
  d.lut_n := lshft(1, (i & 7) + 1);    {number of entries in local color table}

  if d.pix_first_p <> nil then begin   {a previous image buffer exists ?}
    util_mem_ungrab (d.pix_first_p, d.img2_p^.mem_p^); {deallocate old image array}
    end;
  sz := d.size_x * d.size_y * sizeof(d.pix_first_p^); {size of new image array}
  util_mem_grab (sz, d.img2_p^.mem_p^, true, d.pix_first_p); {allocate new array}

  if d.scans_p <> nil then begin       {a previous scan line starts array exists ?}
    util_mem_ungrab (d.scans_p, d.img2_p^.mem_p^); {deallocate old image array}
    end;
  sz := d.size_y * sizeof(d.scans_p^[0]); {size of scan line pointers array}
  util_mem_grab (sz, d.img2_p^.mem_p^, true, d.scans_p); {allocate new array}

  d.lut := d.lut_global;               {init LUT we will use from global color table}
  if lut_present then begin            {LUT supplied for just this image ?}
    read_lut (d, d.lut, err);          {read the local color table info}
    if err then return;
    end;
  if d.transparent then begin          {one pixel value means transparent ?}
    d.lut[transparent].alpha := 0;     {set LUT entry for transparent pixel value}
    d.lut[transparent].red := 0;
    d.lut[transparent].grn := 0;
    d.lut[transparent].blu := 0;
    end;

  d.img_set := false;                  {indicate image data not yet read}
  return;
  end;                                 {done handling image descriptor}
{
**************
*
*   Extension block.
}
block_ext_k: begin
  id_ext := read_byte (d, err);        {get the ID of this extension}
  if err then return;
  case id_ext of
{
******
*
*   Extension block: Graphic Control Extension.
}
ext_gcont_k: begin
  i := read_byte_data (d, err);        {read packed fields byte}
  if err then return;
  d.transparent := (i & 1) <> 0;       {TRUE if a transparent color specified}

  discard( read_byte_data (d, err) );  {ignore delay time low byte}
  if err then return;
  discard( read_byte_data (d, err) );  {ignore delay time high byte}
  if err then return;

  transparent := read_byte_data (d, err); {save pixel value to indicate transparent}
  if err then return;
  end;                                 {done handling graphic control extension}
{
******
*
*   Extension block: Comment Extension.
}
ext_comm_k: begin
  end;
{
******
*
*   Extension block: Plain Text Extension.
}
ext_text_k: begin
  end;
{
******
*
*   Extension block: Application Extension.
}
ext_app_k: begin
  end;
{
******
*
*   Common code after reading all expected data for an extension.  We also
*   end up here if we don't recognize the extension (or haven't implemented
*   it).  We assume the body of all extensions are made up of data sub-blocks.
*   We keep reading until the end of the data sub-blocks.
}
    end;                               {end of all extension type cases}

  repeat                               {read until hit end of data sub blocks}
    discard( read_byte_data(d, err));
    until err;
  if not file_eof(d.stat) then return; {encountered a hard error ?}
  err := false;                        {no error, everything normal}
  end;                                 {end of extension block case}
{
**************
*
*   Trailer.
}
block_trail_k: begin
  sys_stat_set (file_subsys_k, file_stat_eof_k, d.stat); {indicate END OF FILE}
  err := true;
  return;
  end;
{
**************
*
*   Unrecognized GIF stream block ID.
}
otherwise
    sys_stat_set (                     {indicate image file format error}
      img_subsys_k, img_err_file_bad_k, d.stat);
    sys_stat_parm_vstr (d.conn.tnam, d.stat); {file name}
    sys_stat_parm_int (id_block, d.stat); {offending data value}
    sys_stat_parm_int (0, d.stat);     {file offset not implemented right now}
    err := true;
    return;                            {return with hard error}
    end;                               {end of GIF block type cases}

  goto loop_item;                      {back to process next GIF block}
  end;
{
****************************************************
*
*   Subroutine READ_SCAN1 (IMG, SCAN, STAT)
*
*   Read the next scan line from the current image into SCAN.
}
procedure read_scan1 (                 {read next scan line as format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan1_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  internal;

var
  di_p: img_conn2_p_t;                 {pointer to internal IMG data block}
  d_p: data_read_p_t;                  {pointer to our private GIF read data block}
  i: sys_int_machine_t;                {scratch integer and loop counter}
  p: ^char;                            {pointer to current source pixel}
  err: boolean;                        {TRUE on error from local low level routine}

label
  error;

begin
  sys_error_none (stat);
  di_p := img.data_p;                  {get pointer to internal IMG lib data block}
  d_p := di_p^.data_p;                 {get pointer to our private GIF read data}
  with d_p^: d do begin                {D is our private GIF read data block}
{
*   Make sure the image has been read in.  It will be read into the local
*   pixels array on the first call to this routine for each image.
}
  if not d.img_set then begin          {image not yet read into pixel array ?}
    read_image (d, err);               {read the image into the pixels array}
    if err then goto error;
    d.img_set := true;                 {indicate that the image has been read in}
    end;

  if img.next_y >= d.size_y then begin {past the image end ?}
    sys_stat_set (file_subsys_k, file_stat_eof_k, d.stat); {indicate END OF FILE}
    return;
    end;

  p := d.scans_p^[img.next_y];         {init pointer to start of GIF scan line}
  for i := 0 to d.size_x-1 do begin    {once for each pixel on the scan line}
    scan[i] := d.lut[ord(p^)];         {translate and fill in this pixel}
    p := univ_ptr(                     {advance pointer to next GIF source pixel}
      sys_int_adr_t(p) + sizeof(p^));
    end;                               {back for next pixel on the scan line}

  img.next_y := img.next_y + 1;        {update next scan line that we will return}
  return;                              {normal return}
{
*   Error exit.
}
error:                                 {error from low level routine, D.STAT set}
  stat := d.stat;                      {copy error descriptor to call argument}
  end;                                 {done with D abbreviation}
  end;
{
****************************************************
*
*   Subroutine READ_REWIND (IMG, STAT)
*
*   Reset the reading state so that the next scan line read is the first
*   scan line in the current image.
}
procedure read_rewind (                {rewind so next read gets first scan line}
  in out  img: img_conn_t;             {user handle to this image file}
  out     stat: sys_err_t);            {error return code}
  internal;

begin
  sys_error_none (stat);
  img.next_y := 0;
  end;
{
****************************************************
*
*   Subroutine READ_CLOSE (IMG, STAT)
*
*   Close the GIF input file and deallocate resources used in reading it.
}
procedure read_close (                 {close GIF file connection}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}
  internal;

var
  di_p: img_conn2_p_t;                 {pointer to internal IMG data block}
  d_p: data_read_p_t;                  {pointer to our private GIF read data block}

begin
  sys_error_none (stat);
  di_p := img.data_p;                  {get pointer to internal IMG lib data block}
  d_p := di_p^.data_p;                 {get pointer to our private GIF read data}
  with d_p^: d do begin                {D is our private GIF read data block}

  if not d.buf_eof then begin          {GIF file not closed yet ?}
    file_close (d.conn);               {close GIF file}
    end;

  end;                                 {done with D abbreviation}
  end;
{
****************************************************
*
*   Subroutine IMG_D_GIF_OPEN_READ (FNAM, IMG, STAT)
*
*   Open the indicated GIF file for read.
}
procedure img_d_gif_open_read (        {open GIF file for read}
  in      fnam: univ string_var_arg_t; {image file name, may have extension}
  in out  img: img_conn_t;             {handle to new image file connection}
  out     stat: sys_err_t);            {completion status code}

var
  di_p: img_conn2_p_t;                 {pointer to internal IMG data block}
  d_p: data_read_p_t;                  {pointer to our private GIF read data block}
  i: sys_int_machine_t;                {scratch integer and loop counter}
  lut_present: boolean;                {TRUE on global color table present}
  c: char;                             {scratch character}
  err: boolean;                        {TRUE on error from low level routine}

label
  abort0, abort1, abort2, abort3;

begin
  sys_error_none (stat);
  di_p := img.data_p;                  {get pointer to internal IMG lib data block}
  util_mem_grab (                      {allocate our private GIF reading block}
    sizeof(d_p^),                      {amount of memory to allocate}
    di_p^.mem_p^,                      {parent memory context}
    true,                              {we may need to individually deallocate this}
    d_p);                              {returned pointer to the new memory}
  di_p^.data_p := d_p;                 {save pointer to our private state block}
  with d_p^: d do begin                {D is our private GIF reading state block}

  string_fnam_extend (fnam, '.gif', img.tnam); {require the .GIF suffix}
  file_open_read_bin (                 {try to open the GIF file for read}
    img.tnam,                          {file name}
    '',                                {file name suffix}
    d.conn,                            {returned file connection handle}
    stat);
  if sys_error(stat) then goto abort0;
{
*   GIF file open successfully.
}
  d.img_p := addr(img);                {init some state before first GIF read}
  d.img2_p := di_p;
  d.pix_first_p := nil;
  d.scans_p := nil;
  d.data_n := 0;
  d.buf_n := 0;
  d.buf_ind := 0;
  d.buf_eof := false;
  sys_error_none (d.stat);
{
*   Read the GIF header.
}
  c := chr(read_byte (d, err));        {read "G" signature byte}
  if err then goto abort3;
  if c <> 'G' then begin
    %debug; writeln ('Error in first GIF signature byte.');
    goto abort2;
    end;
  c := chr(read_byte (d, err));        {read "I" signature byte}
  if err then goto abort3;
  if c <> 'I' then begin
    %debug; writeln ('Error in second GIF signature byte.');
    goto abort2;
    end;
  c := chr(read_byte (d, err));        {read "F" signature byte}
  if err then goto abort3;
  if c <> 'F' then begin
    %debug; writeln ('Error in third GIF signature byte.');
    goto abort2;
    end;

  discard( read_byte(d, err) );        {ignore GIF version bytes}
  if err then goto abort3;
  discard( read_byte(d, err) );
  if err then goto abort3;
  discard( read_byte(d, err) );
  if err then goto abort3;
{
*   Read logical screen descriptor.
}
  discard( read_i16(d, err) );         {ignore logical screen width}
  if err then goto abort3;
  discard( read_i16(d, err) );         {ignore logical screen height}
  if err then goto abort3;

  i := read_byte(d, err);              {read packed fields byte}
  if err then goto abort3;
  lut_present := (i & 16#80) <> 0;     {TRUE if global color table follows}
  d.lut_n := lshft(1, (i & 7) + 1);    {number of entries in global color table}

  discard( read_byte(d, err) );        {ignore background color index}
  if err then goto abort3;

  i := read_byte(d, err);              {read pixel aspect ratio byte}
  if i = 0
    then begin                         {no pixel aspect ratio specified}
      d.pasp := 1.0;                   {assume square pixels}
      end
    else begin                         {a fixed pixel aspect ratio was specified}
      d.pasp := (i + 15) / 64.0;
      end
    ;
{
*   Set the global color table.
}
  if lut_present
    then begin                         {an explicit color table is supplied}
      read_lut (d, d.lut_global, err); {read the color table from the GIF file}
      if err then goto abort3;
      end
    else begin                         {no color table supplied, pick a default}
      for i := 0 to 255 do begin
        d.lut_global[i].alpha := 255;  {set to gray ramp}
        d.lut_global[i].red := i;
        d.lut_global[i].grn := i;
        d.lut_global[i].blu := i;
        end;
      end
    ;
{
*   Read items from the GIF file until all the control information for the
*   first image has been gathered.  We wait to read the pixels until the
*   app reads the first scan line.  This allows us to return from the OPEN
*   call much quicker.  Some apps (IMAGE_INFO, for example) only call OPEN
*   to get information about the image.
}
  read_items (d, err);                 {get all control info for next image}
  if err then goto abort3;
{
*   We have all the info for the first image.  Now fill in IMG, the app-visible
*   connection descriptor.
}
  img.x_size := d.size_x;
  img.y_size := d.size_y;
  img.aspect := d.pasp * d.size_x / d.size_y;
  img.next_y := 0;
  if d.transparent
    then img.bits_alpha := 1
    else img.bits_alpha := 0;
  img.bits_red := 8;
  img.bits_grn := 8;
  img.bits_blu := 8;
  img.bits_max := 8;
  string_vstring (img.file_type, 'gif', 3);
  img.read_write := file_rw_read_k;
  string_generic_fnam (d.conn.gnam, '.gif', img.gnam);
  string_copy (d.conn.tnam, img.tnam);
{
*   Fill in our parts of the internal IMG library connection descriptor.
}
  di_p^.read_scan1 := addr(read_scan1);
  di_p^.rewind := addr(read_rewind);
  di_p^.close := addr(read_close);
  return;                              {normal return}
{
*   Error exits.
}
abort3:                                {low level routine error, D.STAT set}
  stat := d.stat;
  goto abort1;

abort2:                                {GIF file format error}
  sys_stat_set (img_subsys_k, img_err_file_bad_k, stat);
  sys_stat_parm_vstr (d.conn.tnam, stat);
  sys_stat_parm_int (0, stat);
  sys_stat_parm_int (0, stat);

abort1:                                {error, STAT already set, GIF file open}
  file_close (d.conn);                 {close the GIF file}

abort0:                                {error, STAT already set, private state alloc}
  util_mem_ungrab (di_p^.data_p, di_p^.mem_p^); {deallocate private GIF state block}
  end;                                 {done with D abbreviation}
  end;
