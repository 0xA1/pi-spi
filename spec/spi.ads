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

package SPI is

   type Mode_Type is new uint32_t;

   Mode_Loop      : constant Mode_Type := 16#0000_0020#;
   Mode_CPHA      : constant Mode_Type := 16#0000_0001#;
   Mode_CPOL      : constant Mode_Type := 16#0000_0002#;
   Mode_LSB_First : constant Mode_Type := 16#0000_0008#;
   Mode_CS_High   : constant Mode_Type := 16#0000_0004#;
   Mode_No_CS     : constant Mode_Type := 16#0000_0040#;
   Mode_3Wire     : constant Mode_Type := 16#0000_0010#;
   Mode_Ready     : constant Mode_Type := 16#0000_0080#;
   Mode_TX_Dual   : constant Mode_Type := 16#0000_0100#;
   Mode_TX_Quad   : constant Mode_Type := 16#0000_0200#;
   Mode_RX_Dual   : constant Mode_Type := 16#0000_0400#;
   Mode_RX_Quad   : constant Mode_Type := 16#0000_0800#;

   Error_Mode     : exception;
   Error_Bits     : exception;
   Error_Freq     : exception;
   Error_Device   : exception;
   Error_Xfer     : exception;
   Error_Unknown  : exception;

   type SPI_Device is record
      fd       : aliased int;       -- ../include/spi_driver.h:32
      clk_freq : aliased uint32_t;  -- ../include/spi_driver.h:33
      mode     : aliased uint32_t;  -- ../include/spi_driver.h:34
      c_delay  : aliased uint16_t;  -- ../include/spi_driver.h:35
      bits     : aliased uint8_t;   -- ../include/spi_driver.h:36
   end record;
   pragma Convention (C_Pass_By_Copy, SPI_Device);  -- ../include/spi_driver.h:31

   procedure Open(device       : access SPI_Device;
                  device_path  : Interfaces.C.Strings.chars_ptr;
                  mode         : Mode_Type;
                  bits         : uint8_t;
                  freq         : uint32_t);

   procedure Transfer(device          : access SPI_Device;
                      transmit_buffer : access uint8_t;
                      receive_buffer  : access uint8_t;
                      c_delay         : uint16_t;
                      length          : stddef_h.size_t);

   procedure Close(device : access SPI_Device);

   private 

   --   @brief   Initialize and configure a SPI device.
   --   @param[in,out] spi    Address of a SPI device structure.
   --   @param[in]     dev    Full path to SPI device.
   --   @param[in]     mode   SPI_MODE_LOOP       Loopback
   --                         SPI_MODE_CPHA       Clock Phase
   --                         SPI_MODE_CPOL       Clock Polarity
   --                         SPI_MODE_LSB_FIRST  Least Significant Bit First
   --                         SPI_MODE_CS_HIGH    Chip Select Active High
   --                         SPI_MODE_NO_CS      No Chip Select
   --                         SPI_MODE_3WIRE      SI/SO Signals Shared
   --                         SPI_MODE_READY      Slave may pull down to pause
   --                         SPI_MODE_TX_DUAL    Dual transfer
   --                         SPI_MODE_TX_QUAD    Quad transfer
   --   @param[in]     bits   Number of bits in a transfer
   --   @param[in]     freq   Clock frequency (Hz)
   --   @return        SPI_SUCCESS     SPI device opened successfully
   --   @return        SPI_ERROR_MODE  Error: Cannot set mode
   --   @return        SPI_ERROR_BITS  Error: Cannot set bits
   --   @return        SPI_ERROR_FREQ  Error: Cannot set frequency
   --   @return        SPI_ERROR_DEV   Error: Cannot open device
   -- 
   function c_open
     (device       : access SPI_Device;
      device_path  : Interfaces.C.Strings.chars_ptr;
      mode         : uint32_t;
      bits         : uint8_t;
      freq         : uint32_t) return int;  -- ../include/pi-spi_driver.h:85
   pragma Import (CPP, c_open, "_Z15spi_driver_openP6spidevPcjhj");
                                

   --   @brief  Send and receive some data.
   --   @param[in,out] spi              Address of a SPI device structure.
   --   @param[in]     transmit_buffer  Address of the first byte to transmit
   --   @param[out]    receive_buffer   Address to write data read in from SPI
   --   @param[in]     delay            Delay in us before the transmission
   --   @param[in]     length           Number of bytes to read/write
   --   @return        SPI_SUCCESS      Operation completed successfully
   --   @return        SPI_ERROR_XFER   Error: Could not send data
   -- 
   function c_transfer
     (device          : access SPI_Device;
      transmit_buffer : access uint8_t;
      receive_buffer  : access uint8_t;
      c_delay         : uint16_t;
      length          : stddef_h.size_t) return int;  -- ../include/spi_driver.h:101
   pragma Import (CPP, c_transfer, "_Z19spi_driver_transferP6spidevPhS1_tj");

   --   @brief  Close SPI device.
   --   @param[in,out] spi    Address of a SPI device structure.
   -- 
   procedure c_close (device : access SPI_Device);  -- ../include/spi_driver.h:111
   pragma Import (CPP, c_close, "_Z16spi_driver_closeP6spidev");

end SPI;
