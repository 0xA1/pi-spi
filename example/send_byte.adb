with ada.text_io;          use ada.text_io;
with spi;                  use spi;
with stdint_h;             use stdint_h;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with ada.command_line;     use ada.command_line;
with ada.exceptions;       use ada.exceptions;

procedure Send_Byte is

   device : aliased SPI_Device;
   tx_buf : aliased uint8_t;
   rx_buf : aliased uint8_t;

   procedure display_usage is
   begin
      put_line("Usage: ada_send_byte [DEVICE_PATH] [BYTE]\n");
      put_line("  DEVICE_PATH: Full path to SPI device file.\n");
      put_line("  BYTE:        Byte to send (in hexidecimal)\n");
   end display_usage;

begin

   -- Validate argument count --
   if argument_count < 2 then
      display_usage;
      return;
   end if;

   -- Convert byte parameter to binary --
   tx_buf := uint8_t'value("16#" & argument(2) & "#");

   -- Open the SPI device --
   Open(Device      => device'access,
        Device_Path => New_String(argument(1)),
        Mode => 0,
        Bits => 8,
        Freq => 20000);

   -- Transfer one byte over SPI --

   Transfer(Device          => device'access,
            Transmit_Buffer => tx_buf'access,
            Receive_Buffer  => rx_buf'access,
            C_Delay         => 0,
            Length          => 1);

   -- Close the SPI device --
   Close(Device => device'access);

exception

  when error: others =>
    put(Exception_Information(error));

end Send_Byte;
