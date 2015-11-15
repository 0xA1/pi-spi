pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package arm_linux_gnueabihf_asm_posix_types_h is

  -- *  arch/arm/include/asm/posix_types.h
  -- *
  -- *  Copyright (C) 1996-1998 Russell King.
  -- *
  -- * This program is free software; you can redistribute it and/or modify
  -- * it under the terms of the GNU General Public License version 2 as
  -- * published by the Free Software Foundation.
  -- *
  -- *  Changelog:
  -- *   27-06-1996	RMK	Created
  --  

  -- * This file is generally used by user-level software, so you need to
  -- * be a little careful about namespace pollution etc.  Also, we cannot
  -- * assume GCC is being used.
  --  

   subtype uu_kernel_mode_t is unsigned_short;  -- /usr/include/arm-linux-gnueabihf/asm/posix_types.h:22

   subtype uu_kernel_ipc_pid_t is unsigned_short;  -- /usr/include/arm-linux-gnueabihf/asm/posix_types.h:25

   subtype uu_kernel_uid_t is unsigned_short;  -- /usr/include/arm-linux-gnueabihf/asm/posix_types.h:28

   subtype uu_kernel_gid_t is unsigned_short;  -- /usr/include/arm-linux-gnueabihf/asm/posix_types.h:29

   subtype uu_kernel_old_dev_t is unsigned_short;  -- /usr/include/arm-linux-gnueabihf/asm/posix_types.h:32

end arm_linux_gnueabihf_asm_posix_types_h;
