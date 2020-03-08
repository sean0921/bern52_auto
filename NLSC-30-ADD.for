c **********************************************************************
c * NLSC-30-ADD.for                                                    *
c *                                                                    *
c *      This program is used to pre-process some data for BPE         *
c *      of Bernese 5.2 and output *.sum files for TurboNet.           *
c *                                                                    *
c *                 created for 5.2 version by Kwo-Hwa Chen 2017.09.27 *
c *                                                         2017.11.15 *
c *                                                         2018.11.28 *
c **********************************************************************
      program nlsc30add

      implicit none
      character cmm*200,snxname*12,fixsta*4

      open(10,file='GPSDATA-ADD.LST')

      read(10,*) fixsta

      if(fixsta(1:4).eq.'KMNM') cmm='copy/y FIXSTA-KMNM.SIG FIXSTA.SIG'
      if(fixsta(1:4).eq.'LSB0') cmm='copy/y FIXSTA-LSB0.SIG FIXSTA.SIG'
      if(fixsta(1:4).eq.'JUNA') cmm='copy/y FIXSTA-JUNA.SIG FIXSTA.SIG'
      if(fixsta(1:4).eq.'CKSV') cmm='copy/y FIXSTA-CKSV.SIG FIXSTA.SIG'
      if(fixsta(1:4).eq.'S01R') cmm='copy/y FIXSTA-S01R.SIG FIXSTA.SIG'
      if(fixsta(1:4).eq.'HENC') cmm='copy/y FIXSTA-HECN.SIG FIXSTA.SIG'
      call system(cmm)


      call system('md D:\GPSR\ADDNEQ\')
      call system('md D:\GPSR\ADDNEQ\ATM')
      call system('md D:\GPSR\ADDNEQ\BPE')
      call system('md D:\GPSR\ADDNEQ\GRD')
      call system('md D:\GPSR\ADDNEQ\OBS')
      call system('md D:\GPSR\ADDNEQ\ORB')
      call system('md D:\GPSR\ADDNEQ\ORX')
      call system('md D:\GPSR\ADDNEQ\OUT')
      call system('md D:\GPSR\ADDNEQ\RAW')
      call system('md D:\GPSR\ADDNEQ\SOL')
      call system('md D:\GPSR\ADDNEQ\STA')
      call system('md D:\GPSR\ADDNEQ\SUM')


      call system('copy D:\GPSD\NQ0\*.NQ0 D:\GPSR\ADDNEQ\SOL\')
      call system('copy D:\GPSD\NQ0\*.CRD D:\GPSR\ADDNEQ\STA\')
      call system('copy T97_2010.CRD D:\GPSR\ADDNEQ\STA\')
      call system('copy SESSIONS.SES D:\GPSR\ADDNEQ\STA\')
      call system('copy FIXSTA.SIG D:\GPSR\ADDNEQ\STA\')


      cmm='PERL C:\GPSUSER52\SCRIPT\BERN52_ADDNEQ.PL 2018 0011 D:\GPSR\A
     +DDNEQ'
      call system(cmm)

      call system('copy D:\GPSR\ADDNEQ\SOL\ADDNEQ.SNX C:\BERN52\AUTO\')
      call system('dir/b/o C:\BERN52\AUTO\*.SNX > TR2SUM.LST')

      open(20,file='TR2SUM.LST')

      read(20,'(a12)') snxname


      if(snxname(1:2).ne.'  ') then

        call system('copy ADDNEQ.SNX ADD_0011.SNX')

        call system('RDSINEX50 ADD_0011.SNX')

        call system('rename ADD_0011.SUM ADDNEQ.SUM')
        call system('move C:\BERN52\AUTO\*.SUM D:\GPSR\ADDNEQ\SUM')

        call system('rename D:\GPSR\ADDNEQ ADDNEQA')

        call system('del/q C:\BERN52\AUTO\*.SNX')

      else

        call system('rename D:\GPSR\ADDNEQ ADDNEQB')

      end if

      close(10)
      close(20)

      call system('del TR2SUM.LST')

      stop
      end