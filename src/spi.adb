--                                                                            --
--  Copyright (c) 2015, John Leimon                                           --
--                                                                            --
--  Permission to use, copy, modify, and/or distribute this software for any  --
--  purpose with or without fee is hereby granted, provided that the above    --
--  copyright notice and this permission notice appear in all copies.         --
--                                                                            --
--  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES  --
--  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF          --
--  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR   --
--  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    --
--  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN     --
--  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF   --
--  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.            --
--                                                                            --

with Interfaces.C; use Interfaces.C;
with stdint_h;     use stdint_h;
with stddef_h;     use stddef_h;
with Interfaces.C.Strings;

package body SPI is

   procedure Open(device       : access SPI_Device;
                  device_path  : Interfaces.C.Strings.chars_ptr;
                  mode         : Mode_Type;
                  bits         : uint8_t;
                  freq         : uint32_t) is
      result : int;
   begin
      result := c_open(device, device_path, uint32_t(mode), bits, freq);

      case result is
        when 0 =>
           -- Success --
           return;
        when 1 =>
           -- Invalid mode --
           raise Error_Mode;
        when 2 =>
           -- Invalid number of bits --
           raise Error_Bits;
        when 3 =>
           -- Invalid frequency --
           raise Error_Freq;
        when 4 =>
           -- Device Error --
           raise Error_Device;
        when others =>
           -- Undefined Error --
           raise Error_Unknown;
      end case;
   end Open;

   --------------------------------------------------------------------------------

   procedure Transfer(device          : access SPI_Device;
                      transmit_buffer : access uint8_t;
                      receive_buffer  : access uint8_t;
                      c_delay         : uint16_t;
                      length          : stddef_h.size_t) is
      result : int;
   begin
      result := c_transfer(device, transmit_buffer, receive_buffer, c_delay, length);

      case result is
         when 0 => 
            -- Success --
            return;
         when 5 => 
            -- Transfer Error --
            raise Error_Xfer;
         when others =>
            -- Undefined Error --
            raise Error_Unknown;
      end case;
   end Transfer;

   --------------------------------------------------------------------------------

   procedure Close(device : access SPI_Device) is
   begin
      c_close(device);
   end Close;

begin
  null;
end SPI;
