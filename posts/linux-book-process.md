---
title: Linux Boot Process 
---

1. uefi
- post power-on self-test (POST)
- initializes hardwares required for booting
- determine where to load efi from boot entries in the NVRAM
- launches EFI applications, e.g. boot loaders
    usually located `/efi`

2. boot loader 
- loading the kernel with the wanted kernel parameters
- initial RAM disk 

3. initramfs
- to bootstrap the system to the point where it can access the root filesystem (see FHS for details)
    - For this reason, the initramfs only needs to contain the modules necessary to access the root filesystem; it does not need to contain every module one would ever want to use. The majority of modules will be loaded later on by udev, during the init process.

4. init process
- parent of all process
    - systemd in archlinux

# files hierachy

> https://jlk.fjfi.cvut.cz/arch/manpages/man/file-hierarchy.7

References:
> https://wiki.archlinux.org/index.php/Arch_boot_process
> https://refspecs.linuxfoundation.org/FHS_3.0/fhs/index.html
> https://leetcode.com/discuss/interview-question/operating-system/124638/what-happens-in-the-background-from-the-time-you-press-the-Power-button-until-the-Linux-login-prompt-appears