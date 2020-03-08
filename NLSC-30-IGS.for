c **********************************************************************
c * NLSC-30-IGS.for                                                    *
c *                                                                    *
c *      This program is used to pre-process some data for BPE         *
c *      of Bernese 5.2 and output *.sum files for TurboNet.           *
c *                                                                    *
c *                 created for 5.2 version by Kwo-Hwa Chen 2017.09.27 *
c *                                                         2017.11.15 *
c *                                                         2018.09.20 *
c *                                                         2018.11.25 *
c **********************************************************************
      program nlsc30igs

      implicit none
CCC      character buffer*20,cl*80,inf*50
      character cmm*200,campname*7,snxname*12
      character basename*4,doy*3,ss*2,yr*2
CCC      integer*4 nargs

CCC      call getcl(cl)
CCC      call getarg(nargs,buffer,cl)

CCC      inf=buffer

      open(5,file='GPSDATA-IGS.LST')

 100  read(5,*,end=600) campname

      if(campname(1:1).eq.'*') go to 100

      open(15,file='RUN.INP')

      write(15,*) '2'
      write(15,'(a7,1x,a8,1x,a8,a7,a1)') campname(1:7),'D:\GPSR\','D:\GPSD\',campname,'\'

      close(15)

      cmm='dir/b/o D:\GPSD\'//campname//'\*.??O > BASE.LST'
      call system(cmm)

      open(10,file='BASE.LST')
      read(10,'(a4,a3,2a2)') basename,doy,ss,yr
      basename=basename
      close(10)

      cmm='md D:\GPSR\'//campname(1:7)//'\SUM'
      call system(cmm)

      cmm='BERN52-OBS30-IGS RUN.INP'
      call system(cmm)

      cmm='copy D:\GPSR\'//campname(1:7)//'\SOL\*.SNX C:\BERN52\AUTO\'
      call system(cmm)

      cmm='dir/b/o C:\BERN52\AUTO\*.SNX > TR2SUM.LST'
      call system(cmm)

      open(20,file='TR2SUM.LST')


      read(20,'(a12)') snxname

      if(snxname(1:2).ne.'  ') then

        cmm='copy '//snxname//' 20'//yr(1:2)//doy(1:3)//ss(1:1)//'.SNX'
        call system(cmm)

        cmm='RDSINEX50 20'//yr//doy//ss(1:1)//'.SNX'
        call system(cmm)

        cmm='move C:\BERN52\AUTO\*.SUM D:\GPSR\'//campname(1:7)//'\SUM\'
        call system(cmm)
        cmm='rename D:\GPSR\'//campname(1:7)//' '//campname(1:7)//'I'
        call system(cmm)

        cmm='del/q C:\BERN52\AUTO\*.SNX'
        call system(cmm)

      else

        cmm='rename D:\GPSR\'//campname(1:7)//' '//campname(1:7)//'J'
        call system(cmm)

      end if

      close(20)

      go to 100

 600  continue
      cmm='del BASE.LST'
      call system(cmm)
      cmm='del TR2SUM.LST'
      call system(cmm)
      cmm='del RUN.INP'
      call system(cmm)

      close(5)

      stop
      end


CCC      subroutine getarg(numargs,arg,comline)
CCC      implicit real*8(a-h,o-z)
CCC      character  arg(*)*20,comline*(*)
CCC
CCC      n=nblank(comline)
CCC      n=n+1
CCC      j1=0
CCC      j2=0
CCC      numargs=0
CCC
CCC 560  ibegin=j2+1
CCC      do i=ibegin, n
CCC         if (comline(i:i).ne.' ') then
CCC            j1=i
CCC            do k=i,n
CCC               if (comline(k:k).eq.' ') then
CCC                  j2=k-1
CCC                  numargs=numargs+1
CCC                  arg(numargs)=comline(j1:j2)
CCC                  go to 560
CCC               endif
CCC            end do
CCC         endif
CCC      end do
CCC      return
CCC      end

