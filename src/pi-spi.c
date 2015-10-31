////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  pi-spi.c                                                                  //
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
#include <stdint.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/types.h>
#include "pi-spi.h"

////////////////////////////////////////////////////////////////////////////////
// Function Definitions                                                       //
////////////////////////////////////////////////////////////////////////////////

pispi_init(struct spidev *  spi,
           char *           dev,
           uint32_t         mode,
           uint8_t          bits,
           uint32_t         freq)
{
  int       result;
  uint32_t  new_mode;
  uint8_t   new_bits;
  uint32_t  new_freq;

  new_mode = mode;
  new_bits = bits;
  new_freq = freq;

  // Open device //
  spi->fd = open(dev, O_RDWR);
  if (spi->fd == -1) return SPI_ERROR_DEV;

  // Set mode //
  result = ioctl(spi->fd, SPI_IOC_WR_MODE32, &new_mode);
  if (result == -1) return SPI_ERROR_MODE;
  result = ioctl(spi->fd, SPI_IOC_RD_MODE32, &new_mode);
  if (result == -1) return SPI_ERROR_MODE;
  if (new_mode != mode) return SPI_ERROR_MODE;
  spi->mode = mode;

  // Set number of bits in word //
  result = ioctl(spi->fd, SPI_IOC_WR_BITS_PER_WORD, &new_bits);
  if (result == -1) return SPI_ERROR_BITS;
  result = ioctl(spi->fd, SPI_IOC_RD_BITS_PER_WORD, &new_bits);
  if (result == -1) return SPI_ERROR_BITS;
  if (new_bits != bits) return SPI_ERROR_BITS;
  spi->bits = bits;

  // Set clock frequency //
  result = ioctl(spi->fd, SPI_IOC_WR_MAX_SPEED_HZ, &new_freq);
  if (result == -1) return SPI_ERROR_FREQ;
  result = ioctl(spi->fd, SPI_IOC_RD_MAX_SPEED_HZ, &new_freq);
  if (result == -1) return SPI_ERROR_FREQ;
  if (new_freq != freq) return SPI_ERROR_FREQ;
  spi->clk_freq = freq;

  return SPI_SUCCESS;
}

////////////////////////////////////////////////////////////////////////////////

int pispi_transfer(struct spidev *  spi,
                   uint8_t *        transmit_buffer,
                   uint8_t *        receive_buffer,
                   uint16_t         delay,
                   size_t           length)
{
  int                      result;
  struct spi_ioc_transfer  envelope;

  // Pack transmission structure //
  envelope.tx_buf        = (unsigned long) transmit_buffer;
  envelope.rx_buf        = (unsigned long) receive_buffer;
  envelope.len           = length;
  envelope.delay_usecs   = delay;
  envelope.speed_hz      = spi->clk_freq;
  envelope.bits_per_word = spi->bits;

  if (spi->mode & SPI_MODE_TX_DUAL) {
    envelope.tx_nbits = 2;
  } else if (spi->mode & SPI_MODE_TX_QUAD) {
    envelope.tx_nbits = 4;
  } else {
    envelope.tx_nbits = 1;
  }
  
  if (spi->mode & SPI_MODE_RX_DUAL) {
    envelope.rx_nbits = 2;
  } else if (spi->mode & SPI_MODE_RX_QUAD) {
    envelope.rx_nbits = 4;
  } else {
    envelope.rx_nbits = 1;
  }

  // Transfer data //
  result = ioctl(spi->fd, SPI_IOC_MESSAGE(1), &envelope);
  if(result == -1) return SPI_ERROR_XFER;

  return SPI_SUCCESS;
}

////////////////////////////////////////////////////////////////////////////////

void pispi_close(struct spidev *  spi)
{
  close(spi->fd);
}
