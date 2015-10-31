////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  pi-spi.h                                                                  //
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
#ifndef __PI_SPI_H__
#define __PI_SPI_H__

#include <stdint.h>
#include <linux/spi/spidev.h>

////////////////////////////////////////////////////////////////////////////////
// Definitions                                                                //
////////////////////////////////////////////////////////////////////////////////

struct spidev {
  int       fd;
  uint32_t  clk_freq;
  uint32_t  mode;
  uint16_t  delay;
  uint8_t   bits;
};
   
#define SPI_MODE_LOOP                       SPI_LOOP
#define SPI_MODE_CPHA                       SPI_CPHA
#define SPI_MODE_CPOL                       SPI_CPOL
#define SPI_MODE_LSB_FIRST                  SPI_LSB_FIRST
#define SPI_MODE_CS_HIGH                    SPI_CS_HIGH
#define SPI_MODE_NO_CS                      SPI_NO_CS
#define SPI_MODE_3WIRE                      SPI_3WIRE
#define SPI_MODE_READY                      SPI_READY
#define SPI_MODE_TX_DUAL                    SPI_TX_DUAL
#define SPI_MODE_TX_QUAD                    SPI_TX_QUAD
#define SPI_MODE_RX_DUAL                    SPI_RX_DUAL
#define SPI_MODE_RX_QUAD                    SPI_RX_QUAD
                                            
#define SPI_SUCCESS                         0
#define SPI_ERROR_MODE                      1
#define SPI_ERROR_BITS                      2
#define SPI_ERROR_FREQ                      3
#define SPI_ERROR_DEV                       4
#define SPI_ERROR_XFER                      5

////////////////////////////////////////////////////////////////////////////////
// Function Prototypes                                                        //
////////////////////////////////////////////////////////////////////////////////

/*
   @brief   Initialize and configure a SPI device.
   @param[in,out] spi    Address of a SPI device structure.
   @param[in]     dev    Full path to SPI device.
   @param[in]     mode   SPI_MODE_LOOP       Loopback
                         SPI_MODE_CPHA       Clock Phase
                         SPI_MODE_CPOL       Clock Polarity
                         SPI_MODE_LSB_FIRST  Least Significant Bit First
                         SPI_MODE_CS_HIGH    Chip Select Active High
                         SPI_MODE_NO_CS      No Chip Select
                         SPI_MODE_3WIRE      SI/SO Signals Shared
                         SPI_MODE_READY      Slave may pull down to pause
                         SPI_MODE_TX_DUAL    Dual transfer
                         SPI_MODE_TX_QUAD    Quad transfer
   @param[in]     bits   Number of bits in a transfer
   @param[in]     freq   Clock frequency (Hz)
   @return        SPI_SUCCESS     SPI device opened successfully
   @return        SPI_ERROR_MODE  Error: Cannot set mode
   @return        SPI_ERROR_BITS  Error: Cannot set bits
   @return        SPI_ERROR_FREQ  Error: Cannot set frequency
   @return        SPI_ERROR_DEV   Error: Cannot open device
*/
int pispi_open(struct spidev *  spi,
               char *           dev,
               uint32_t         mode,
               uint8_t          bits,
               uint32_t         freq);

/*
   @brief  Send and receive some data.
   @param[in,out] spi              Address of a SPI device structure.
   @param[in]     transmit_buffer  Address of the first byte to transmit
   @param[out]    receive_buffer   Address to write data read in from SPI
   @param[in]     delay            Delay in us before the transmission
   @param[in]     length           Number of bytes to read/write
   @return        SPI_SUCCESS      Operation completed successfully
   @return        SPI_ERROR_XFER   Error: Could not send data
*/
int pispi_transfer(struct spidev *  spi,
                   uint8_t *        transmit_buffer,
                   uint8_t *        receive_buffer,
                   uint16_t         delay,
                   size_t           length);

/*
   @brief  Close SPI device.
   @param[in,out] spi    Address of a SPI device structure.
*/
void pispi_close(struct spidev *  spi);

#endif
