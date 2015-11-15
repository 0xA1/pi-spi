pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with asm_generic_int_ll64_h;

package linux_spi_spidev_h is

   --  unsupported macro: SPI_CPHA 0x01
   --  unsupported macro: SPI_CPOL 0x02
   --  unsupported macro: SPI_MODE_0 (0|0)
   --  unsupported macro: SPI_MODE_1 (0|SPI_CPHA)
   --  unsupported macro: SPI_MODE_2 (SPI_CPOL|0)
   --  unsupported macro: SPI_MODE_3 (SPI_CPOL|SPI_CPHA)
   --  unsupported macro: SPI_CS_HIGH 0x04
   --  unsupported macro: SPI_LSB_FIRST 0x08
   --  unsupported macro: SPI_3WIRE 0x10
   --  unsupported macro: SPI_LOOP 0x20
   --  unsupported macro: SPI_NO_CS 0x40
   --  unsupported macro: SPI_READY 0x80
   --  unsupported macro: SPI_TX_DUAL 0x100
   --  unsupported macro: SPI_TX_QUAD 0x200
   --  unsupported macro: SPI_RX_DUAL 0x400
   --  unsupported macro: SPI_RX_QUAD 0x800

   SPI_IOC_MAGIC : aliased constant Character := 'k';  --  /usr/include/linux/spi/spidev.h:54
   --  arg-macro: function SPI_MSGSIZE ((((N)*(sizeof (struct spi_ioc_transfer))) < (1 << _IOC_SIZEBITS)) ? ((N)*(sizeof (struct spi_ioc_transfer))) : 0
   --    return (((N)*(sizeof (struct spi_ioc_transfer))) < (1 << _IOC_SIZEBITS)) ? ((N)*(sizeof (struct spi_ioc_transfer))) : 0;
   --  arg-macro: procedure SPI_IOC_MESSAGE _IOW(SPI_IOC_MAGIC, 0, char(SPI_MSGSIZE(N)))
   --    _IOW(SPI_IOC_MAGIC, 0, char(SPI_MSGSIZE(N)))
   --  unsupported macro: SPI_IOC_RD_MODE _IOR(SPI_IOC_MAGIC, 1, __u8)
   --  unsupported macro: SPI_IOC_WR_MODE _IOW(SPI_IOC_MAGIC, 1, __u8)
   --  unsupported macro: SPI_IOC_RD_LSB_FIRST _IOR(SPI_IOC_MAGIC, 2, __u8)
   --  unsupported macro: SPI_IOC_WR_LSB_FIRST _IOW(SPI_IOC_MAGIC, 2, __u8)
   --  unsupported macro: SPI_IOC_RD_BITS_PER_WORD _IOR(SPI_IOC_MAGIC, 3, __u8)
   --  unsupported macro: SPI_IOC_WR_BITS_PER_WORD _IOW(SPI_IOC_MAGIC, 3, __u8)
   --  unsupported macro: SPI_IOC_RD_MAX_SPEED_HZ _IOR(SPI_IOC_MAGIC, 4, __u32)
   --  unsupported macro: SPI_IOC_WR_MAX_SPEED_HZ _IOW(SPI_IOC_MAGIC, 4, __u32)
   --  unsupported macro: SPI_IOC_RD_MODE32 _IOR(SPI_IOC_MAGIC, 5, __u32)
   --  unsupported macro: SPI_IOC_WR_MODE32 _IOW(SPI_IOC_MAGIC, 5, __u32)

  -- * include/linux/spi/spidev.h
  -- *
  -- * Copyright (C) 2006 SWAPP
  -- *	Andrea Paterniani <a.paterniani@swapp-eng.it>
  -- *
  -- * This program is free software; you can redistribute it and/or modify
  -- * it under the terms of the GNU General Public License as published by
  -- * the Free Software Foundation; either version 2 of the License, or
  -- * (at your option) any later version.
  -- *
  -- * This program is distributed in the hope that it will be useful,
  -- * but WITHOUT ANY WARRANTY; without even the implied warranty of
  -- * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  -- * GNU General Public License for more details.
  -- *
  -- * You should have received a copy of the GNU General Public License
  -- * along with this program; if not, write to the Free Software
  -- * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
  --   

  -- User space versions of kernel symbols for SPI clocking modes,
  -- * matching <linux/spi/spi.h>
  --  

  ----------------------------------------------------------------------------- 
  -- IOCTL commands  
  --*
  -- * struct spi_ioc_transfer - describes a single SPI transfer
  -- * @tx_buf: Holds pointer to userspace buffer with transmit data, or null.
  -- *	If no data is provided, zeroes are shifted out.
  -- * @rx_buf: Holds pointer to userspace buffer for receive data, or null.
  -- * @len: Length of tx and rx buffers, in bytes.
  -- * @speed_hz: Temporary override of the device's bitrate.
  -- * @bits_per_word: Temporary override of the device's wordsize.
  -- * @delay_usecs: If nonzero, how long to delay after the last bit transfer
  -- *	before optionally deselecting the device before the next transfer.
  -- * @cs_change: True to deselect device before starting the next transfer.
  -- *
  -- * This structure is mapped directly to the kernel spi_transfer structure;
  -- * the fields have the same meanings, except of course that the pointers
  -- * are in a different address space (and may be of different sizes in some
  -- * cases, such as 32-bit i386 userspace over a 64-bit x86_64 kernel).
  -- * Zero-initialize the structure, including currently unused fields, to
  -- * accommodate potential future updates.
  -- *
  -- * SPI_IOC_MESSAGE gives userspace the equivalent of kernel spi_sync().
  -- * Pass it an array of related transfers, they'll execute together.
  -- * Each transfer may be half duplex (either direction) or full duplex.
  -- *
  -- *	struct spi_ioc_transfer mesg[4];
  -- *	...
  -- *	status = ioctl(fd, SPI_IOC_MESSAGE(4), mesg);
  -- *
  -- * So for example one transfer might send a nine bit command (right aligned
  -- * in a 16-bit word), the next could read a block of 8-bit data before
  -- * terminating that command by temporarily deselecting the chip; the next
  -- * could send a different nine bit command (re-selecting the chip), and the
  -- * last transfer might write some register values.
  --  

   type spi_ioc_transfer is record
      tx_buf : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/spi/spidev.h:90
      rx_buf : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/spi/spidev.h:91
      len : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/spi/spidev.h:93
      speed_hz : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/spi/spidev.h:94
      delay_usecs : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/spi/spidev.h:96
      bits_per_word : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/spi/spidev.h:97
      cs_change : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/spi/spidev.h:98
      tx_nbits : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/spi/spidev.h:99
      rx_nbits : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/spi/spidev.h:100
      pad : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/spi/spidev.h:101
   end record;
   pragma Convention (C_Pass_By_Copy, spi_ioc_transfer);  -- /usr/include/linux/spi/spidev.h:89

  -- If the contents of 'struct spi_ioc_transfer' ever change
  --	 * incompatibly, then the ioctl number (currently 0) must change;
  --	 * ioctls with constant size fields get a bit more in the way of
  --	 * error checking than ones (like this) where that field varies.
  --	 *
  --	 * NOTE: struct layout is the same in 64bit and 32bit userspace.
  --	  

  -- not all platforms use <asm-generic/ioctl.h> or _IOC_TYPECHECK() ...  
  -- Read / Write of SPI mode (SPI_MODE_0..SPI_MODE_3) (limited to 8 bits)  
  -- Read / Write SPI bit justification  
  -- Read / Write SPI device word length (1..N)  
  -- Read / Write SPI device default max speed hz  
  -- Read / Write of the SPI mode field  
end linux_spi_spidev_h;
