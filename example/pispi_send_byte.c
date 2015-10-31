////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  pispi_send_byte.c                                                         //
//                                                                            //
//  Copyright (c) 2015, John Leimon                                           //
//                                                                            //
//  Permission to use, copy, modify, and/or distribute this software for any  //
//  purpose with or without fee is hereby granted, provided that the above    //
//  copyright notice and this permission notice appear in all copies.         //
//                                                                            //
//  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES  //
//  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF          //
//  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR   //
//  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    //
//  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN     //
//  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF   //
//  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
#include <stdio.h>
#include "pi-spi.h"

void display_usage()
{
  printf("Usage: pispi_send_byte [DEVICE_PATH] [BYTE]\n");
  printf("  BYTE:        Byte to send (in hexidecimal)\n");
  printf("  DEVICE_PATH: Full path to SPI device file.\n");
}

int main(int  argc, char *  argv)
{
  int            result;
  struct spidev  spi;

  // Initializing the SPI device //

  result = pispi_init(&spi, "/dev/spidev0.1", 0, 8, 20000);

  printf("pispi_init() = %d", result);

  switch(result) {
    case SPI_SUCCESS:
      printf(" [Success]\n");
      break;
    case SPI_ERROR_MODE:
      printf(" [Error: Mode]\n");
      return 1;
    case SPI_ERROR_BITS:
      printf(" [Error: Bits]\n");
      return 2;
    case SPI_ERROR_FREQ:
      printf(" [Error: Clock Frequency]\n");
      return 3;
    case SPI_ERROR_DEV:
      printf(" [Error: Device]\n");
      return 4;
    default:
      printf(" [Unknown Error] -- (%d)\n", result);
      return 5;
  }

  {
    // Transfer one byte over SPI //

    uint8_t transmit_buffer;
    uint8_t receive_buffer;

    transmit_buffer = 0xA1;

    result = pispi_transfer(&spi,
                            &transmit_buffer,
                            &receive_buffer,
                            0,
                            1);

    printf("pispi_transfer() = %d\n", result);
  }

  // Close the SPI device //

  pispi_close(&spi);

  return 0;
}
