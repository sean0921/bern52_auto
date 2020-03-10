c **********************************************************************
c * BERN52-OBS30-IGS.for                                               *
c *                                                                    *
c *      This program is used to execute BPE of Bernese 5.2 easier     *
c *                                                                    *
c *                 created for 5.0 version by kuo-en ching 2007.04.26 *
c *                modified for 5.2 version by Kwo-Hwa Chen 2018.09.20 *
c **********************************************************************
      program bern52obs30igs

      implicit none
      character buffer*20,cl*80
      character inf*50,infile*60,camp*7,apath*200,opath*200,spath*200
      character s1*200,s2*200,s3*200,cyc*2,mmc*2,ddc*2,sta*4,rep*200
      character tmp(100)*4,fline(100)*200
      character,allocatable::line(:)*100,fixsta(:)*4
      double precision,allocatable::fixcvs(:,:),epo(:)
      integer nargs
      integer i,j,ii,n,ty,stat,yy,mm,dd,doy,doy1(12),doy2(12),tinv,cy
      integer nf,nl,tty,parafix,nnf
      common parafix
      data doy1 /0,31,59,90,120,151,181,212,243,273,304,334/
      data doy2 /0,31,60,91,121,152,182,213,244,274,305,335/

CC      write(*,'(" Input file name:  ")')
CC      read(*,'(a\)')inf

      call get_command_argument(1, cl)
      call getarg_local(nargs,buffer,cl)

      inf=buffer

      !! read "FIXSTA.DAT" *********************************************
      do i=1,100
        tmp(i)=''
      end do
      open(11,file='C:\BERN52\AUTO\FIXSTA.DAT',status='old')
      read(11,*)parafix
      read(11,*)
      nf=0
      nl=0
      stat=0
      do while(stat==0)
        nl=nl+1
        read(11,'(a)',iostat=stat)fline(nl)
        if(stat/=0) exit
        if(fline(nl)(1:1)=='!') exit
        read(fline(nl),*) sta
        tty=0
        do i=1,100
          if(tmp(i)==sta) then
            tty=1
            exit
          else if(tmp(i)=='') then
            exit
          end if
        end do
        if(tty==0) then
          nf=nf+1
          tmp(nf)=sta
        end if
      end do
      close(11)
      nl=nl-1
      allocate(fixsta(nf),fixcvs(nf,9),epo(nf))

      !! re-set the path of input file *********************************
      n=0
      do ii=1,50
       if(inf(ii:ii)/=' ') then
         n=n+1
       else
         exit
       end if
      end do
CC      infile='C:\BERN52\'//inf(1:n)
      infile=inf(1:n)

      !! read input file ***********************************************
      open(11,file=infile,status='old')
      read(11,*)ty

      if(ty==0.or.ty==1) then  ! ty==1 or ty==0 ************************
        read(11,*)yy,mm,dd,tinv,s1,s2,s3
        if(mod(yy,4)==0) then
          doy=doy2(mm)+dd
        else
          doy=doy1(mm)+dd
        end if
        doy=doy-1

        do i=1,tinv
          doy=doy+1
          if((mod(yy,4)==0).and.(doy>366))then
            yy=yy+1
            doy=doy-366
          else if((mod(yy,4)/=0).and.(doy>365))then
            yy=yy+1
            doy=doy-365
          end if
          if((mod(yy,4)==0).and.(doy>doy2(12)))then
            mm=12
            dd=doy-doy2(12)
          else if((mod(yy,4)/=0).and.(doy>doy1(12)))then
            mm=12
            dd=doy-doy1(12)
          else
            do j=1,12
              if((mod(yy,4)==0).and.(doy<=doy2(j)))then
                mm=j-1
                dd=doy-doy2(j-1)
                exit
              else if((mod(yy,4)/=0).and.(doy<=doy1(j)))then
                mm=j-1
                dd=doy-doy1(j-1)
                exit
              end if
            end do
          end if
          apath=s1
          opath=s2
          spath=s3
          if(yy<2000)  cy=yy-1900
          if(yy>=2000) cy=yy-2000
          call int2char3(cy,cyc)
          call int2char3(mm,mmc)
          call int2char3(dd,ddc)
          camp=cyc//mmc//ddc//'P'
          call fixinf(yy,doy,nl,nf,fline,tmp,fixsta,fixcvs,epo)
          if(ty==0) nnf=nf
CC          if(ty==1) call check(yy,mm,dd,doy,camp,apath,opath,spath,nf,
CC     +    fixsta,fixcvs,epo,nnf)
          if(ty==0) ty=1
          write(*,'(" Start to execute the formal process ...",/)')
          call preBPE(yy,mm,dd,doy,nf,camp,apath,opath,spath,fixsta,    fixcvs,nnf)
          call exe_bat(yy,doy,camp,apath,ty,nf,fixsta,fixcvs,epo,nnf)
          do ii=1,200
            if(apath(ii:ii)==' ') then
              exit
            else if(apath(ii:ii)=='/') then
              apath(ii:ii)='\'
            end if
          end do
          rep='ren '//apath(1:ii-1)//'\OUT\FN'//cyc//'???1.OUT FN'//cyc    //'???'//camp(7:7)//'.OUT'
          call system(rep)
        end do

      else if(ty==2) then  ! ty==2 *************************************
        n=0
        stat=0
        do while(stat==0)
          read(11,*,iostat=stat)
          if(stat/=0) exit
          n=n+1
        end do
        rewind(11)
        allocate(line(n))
        read(11,*)
        do i=1,n
          read(11,'(a)')line(i)
        end do
        close(11)
        do i=1,n
          if(line(i)(1:1)=='!') cycle
          read(line(i),*)camp,apath,opath
          spath=opath
          read(camp,'(3i2)')yy,mm,dd
          call int2char3(yy,cyc)
          if(yy>90) yy=yy+1900
          if(yy<90) yy=yy+2000
          if(mod(yy,4)==0) then
            doy=doy2(mm)+dd
          else
            doy=doy1(mm)+dd
          end if
          call fixinf(yy,doy,nl,nf,fline,tmp,fixsta,fixcvs,epo)
CC          call check(yy,mm,dd,doy,camp,apath,opath,spath,nf,fixsta,
CC     +    fixcvs,epo,nnf)
          write(*,'(" Start to execute the formal process ...",/)')
          call preBPE(yy,mm,dd,doy,nf,camp,apath,opath,spath,fixsta,    fixcvs,nnf)
          call exe_bat(yy,doy,camp,apath,ty,nf,fixsta,fixcvs,epo,nnf)
          do ii=1,200
            if(apath(ii:ii)==' ') then
              exit
            else if(apath(ii:ii)=='/') then
              apath(ii:ii)='\'
            end if
          end do
CC          rep='ren '//apath(1:ii-1)//'\OUT\FN'//cyc//'???1.OUT FN'//cyc
CC     +    //'???'//camp(7:7)//'.OUT'
CC          call system(rep)
        end do
        deallocate(line)
      end if  ! end of "ty" judgement ************************************

      deallocate(fixsta,fixcvs,epo)

      stop
      end

c **********************************************************************
c *   subroutine fixinf : get informtion of constrained stations       *
c *                       from "FIXSTA.DAT"                            *
c *     Information includes:                                          *
c *     (1) n: number of constrained stations                          *
c *     (2) fixsta(n): constrained station names                       *
c *     (3) fixcvs(n,9): x, y, z,vx, vy, vz, sx, sy, sz of constrained *
c *                     stations                                       *
c *     (4) epo(n): epoch of the given coordinates for each station    *
c **********************************************************************
      subroutine fixinf(yy,doy,nl,n,line,tmp,fixsta,fixcvs,epo)

      implicit none
      integer i,j,n,nl,nn,yy,doy
      character line(nl)*200,tmp(100)*4,p1*4,fixsta(n)*4
      real*8 fixcvs(n,9),epo(n),yr
      real*8 p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13
      nn=0

      if(mod(yy,4)/=0) yr=real(yy)+real(doy-1)/365.
      if(mod(yy,4)==0) yr=real(yy)+real(doy-1)/366.

      do i=1,nl  ! for tmp
        do j=1,nl  ! for line
          read(line(j),*)p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13
          if(p1==tmp(i).and.yr>p12.and.yr<=p13) then
            nn=nn+1
            fixsta(nn)=p1
            fixcvs(nn,1)=p2
            fixcvs(nn,2)=p3
            fixcvs(nn,3)=p4
            fixcvs(nn,4)=p5
            fixcvs(nn,5)=p6
            fixcvs(nn,6)=p7
            fixcvs(nn,7)=p8
            fixcvs(nn,8)=p9
            fixcvs(nn,9)=p10
            epo(nn)=p11
            exit
          end if
        end do
      end do

      return
      end

c **********************************************************************
c *   subroutine preBPE : preparation for BPE                          *
c *     (1) create 11 folders, including 'ATM', 'BPE', 'GRD', 'MSC',   *
c *         'OBS', 'ORB', 'ORX', 'OUT', 'RAW', 'SOL', and 'STA'        *
c *     (2) copy GPS raw data to folder "RAW"                          *
c *     (3) copy 'C04_yyyy.ERP' and 'IGSWWWWD.SP3' to folder "ORB"     *
c *     (4) create a station file "RINEX.CRD" in the folder "STA"      *
c *     (5) create a session file "SESSIONS.SES" in the folder "STA"   *
c *     (6) create a fixed station file "FIXSTA.SIG" in the folder     *
c *         "STA"                                                      *
c **********************************************************************
      subroutine preBPE(yy,mm,dd,doy,nf,camp,apath,opath,spath,fixsta,fixcvs,nnf)

      implicit none
      integer i,ii,iii,j,yy,mm,dd,doy,nf,nnf,y1,day_plus,gpsweek,weekday
      character camp*7,apath*200,opath*200,spath*200,tmp*100,fod(11)*3
      character dyc*3,mmc*2,ddc*2,yyc*4,y1c*2,inf*50,fixsta(nf)*4,iw*5
      real*8 fixcvs(nf,9)
      data fod /'ATM','BPE','GRD','MSC','OBS','ORB','ORX','OUT','RAW', 'SOL','STA'/

      call int2char(doy,dyc)
      if(yy> 1999) y1=yy-2000
      if(yy<=1999) y1=yy-1900
      call int2char3(y1,y1c)
      !! create folders ************************************************
      do ii=1,200
        if(apath(ii:ii)==' ') then
          apath(ii:ii+6)=camp
          exit
        end if
      end do
      do i=1,11
        tmp='mkdir '//apath(1:ii+6)//'\'//fod(i)
        call system(tmp)
      end do
      !! copy GPS raw data to folder "RAW" *****************************
      do iii=1,200
        if(opath(iii:iii)==' ') then
          opath(iii:iii+12)='????'//dyc//'1.'//y1c//'o'
          exit
        end if
      end do
      tmp='copy/y '//opath(1:iii+12)//apath(1:ii+6)//'\'//fod(9)
      call system(tmp)
      !! copy sp3 file to folder "ORB" *********************************
      day_plus=(yy-1995)*365+doy+int((yy-1993)/4)-1
      gpsweek=int(day_plus/7)+782
      weekday=mod(real(day_plus),7.)
      open(99,file='tmp')
      if(gpsweek<1000) write(99,'("0",i3,i1)') gpsweek,weekday
      if(gpsweek>=1000) write(99,'(i4,i1)') gpsweek,weekday
      rewind(99)
      read(99,'(a5)')iw
      close(99,status='delete')
      do iii=1,200
        if(spath(iii:iii)==' ') then
          spath(iii:iii+12)='ig?'//iw//'.sp3 '
          exit
        end if
      end do
      tmp='copy/y '//spath(1:iii+12)//apath(1:ii+6)//'\'//fod(6)
      call system(tmp)
      !! copy erp file to folder "ORB" *********************************
      call int2char2(yy,yyc)
      tmp='copy/y C:\BERN52\GPS\GEN\C04_'//yyc//'.ERP '//apath(1:ii+6)//'\'//fod(6)
      call system(tmp)
      tmp='copy/y C:\BERN52\AUTO\ITRF2014.CRD '//apath(1:ii+6)//'\'//fod(11)
      call system(tmp)

      tmp='copy/y C:\BERN52\AUTO\ITRF2014.PSD '//apath(1:ii+6)//'\'//fod(11)
      call system(tmp)

      tmp='copy/y C:\BERN52\AUTO\ITRF2014.SIG '//apath(1:ii+6)//'\'//fod(11)
      call system(tmp)

      tmp='copy/y C:\BERN52\AUTO\ITRF2014.STA '//apath(1:ii+6)//'\'//fod(11)
      call system(tmp)

      tmp='copy/y C:\BERN52\AUTO\ITRF2014.VEL '//apath(1:ii+6)//'\'//fod(11)
      call system(tmp)


CC      !! create "RINEX.CRD" ********************************************
CC      inf=apath(1:ii+6)//'\'//fod(11)//'\RINEX.CRD'
CC      open(10,file=inf)
CC      write(10,*)'COORDINATE FILE CREATED BY RXOBV3'
CC      write(10,*)'------------------------------------------------------
CC     +--------------------------'
CC      write(10,*)'LOCAL GEODETIC DATUM: ITRF2008          EPOCH:'
CC      write(10,*)
CC      write(10,*)'NUM  STATION NAME           X (M)          Y (M)
CC     +    Z (M)     FLAG'
CC      write(10,*)
CC      close(10)
      !! create "SESSIONS.SES" *****************************************
      inf=apath(1:ii+6)//'\'//fod(11)//'\SESSIONS.SES'
      call int2char3(mm,mmc)
      call int2char3(dd,ddc)
      open(10,file=inf)
      write(10,*)
      write(10,'("LIST_OF_SESSIONS 1  """,a3,"1"" """,i4," ",a2," ",a2,""" ""00 00 00"" """,i4," ",a2," ",a2,""" ""23 59 59""")')dyc,yy,mmc,ddc,yy,mmc,ddc
      write(10,*)'  ## widget = uniline; check_strlen.1 = 4; check_type.3 = time'
      write(10,*)'  ## check_type.5 = time'
      write(10,*)
      write(10,*)
      write(10,*)'# BEGIN_PANEL NO_CONDITION #####################################################'
      write(10,*)'# SESSION TABLE                         #'
      write(10,*)'#                         #'
      write(10,*)'#    SESSION                START EPOCH   END EPOCH             #'
      write(10,*)'#   IDENTIFIER         yyyy mm dd  hh mm ss        yyyy mm dd  hh mm ss        #'
      write(10,*)'#    > %%%%            %%%%%%%%%%  %%%%%%%%        %%%%%%%%%%  %%%%%%%% <      # LIST_OF_SESSIONS'
      write(10,*)'#                         #'
      write(10,*)'# END_PANEL ####################################################################'
      close(10)
CC      !! create "FIXSTA.SIG" *******************************************
CC      inf=apath(1:ii+6)//'\'//fod(11)//'\FIXSTA.SIG'
CC      open(10,file=inf)
CC      write(10,*)'Station sigma file
CC     +           25-OCT-03 15:46'
CC      write(10,*)'------------------------------------------------------
CC     +--------------------------'
CC      write(10,*)
CC      write(10,*)'Station name            sigma1    sigma2    sigma3
CC     +                          '
CC      write(10,*)'****************       **.****   **.****   **.****
CC     +                          '
CC      do i=1,nnf
CC        write(10,'(a4,16x,3f10.4)')fixsta(i),(fixcvs(i,j),j=7,9)
CC      end do
CC      close(10)

      return
      end

c **********************************************************************
c *   subroutine exe_bat : create a perl batch file and run BPE        *
c **********************************************************************
      subroutine exe_bat(yy,doy,camp,apath,ty,n,fixsta,fixcvs,epo,nnf)

      implicit none
      integer n,ii,yy,doy,ty,nnf,rty
      character yyc*4,dyc*3,camp*7,apath*200,exe1*100,exe2*100,exe3*100
      character fixsta(n)*4
      real*8 fixcvs(n,9),epo(n)

      !! create a perl batch file **************************************
      do ii=1,200
        if(apath(ii:ii)==' ') then
          exit
        else if(apath(ii:ii)=='\') then
          apath(ii:ii)='/'
        end if
      end do

      !! execute the BPE ***********************************************
      call int2char2(yy,yyc)
      call int2char(doy,dyc)
      ! for continuous-mode (sampling rate: 30 sec) calculation
      if(ty==1) exe1='PERL C:\GPSUSER52\SCRIPT\AUTO52_S30A.PL '//yyc//' '//dyc//'1 '//apath
      if(ty==1) exe2='PERL C:\GPSUSER52\SCRIPT\AUTO52_S30B.PL '//yyc//' '//dyc//'1 '//apath
      if(ty==1) exe3='PERL C:\GPSUSER52\SCRIPT\AUTO52_S30C.PL '//yyc//' '//dyc//'1 '//apath
      ! for campaign-mode (sampling rate: 15 sec) calculation
CC      if(ty==2) exe1='PERL C:\GPSUSER52\SCRIPT\AUTO52_S30A.PL '//yyc
CC     +//' '//dyc//'1 '//apath
CC      if(ty==2) exe2='PERL C:\GPSUSER52\SCRIPT\AUTO52_S30B.PL '//yyc
CC     +//' '//dyc//'1 '//apath
      if(ty==2) exe1='PERL C:\GPSUSER52\SCRIPT\BERN52_S30A_IGS.PL '//yyc//' '//dyc//'1 '//apath
      if(ty==2) exe2='PERL C:\GPSUSER52\SCRIPT\BERN52_S30B_IGS.PL '//yyc//' '//dyc//'1 '//apath
CC      if(ty==2) exe3='PERL C:\GPSUSER52\SCRIPT\AUTO52_S30C.PL '//yyc
CC     +//' '//dyc//'1 '//apath
      ! for check-mode (sampling rate: 30 sec) calculation
      if(ty==3) exe1='PERL C:\GPSUSER52\SCRIPT\AUTO52_S30A.PL '//yyc//' '//dyc//'1 '//apath(1:ii-1)//camp
      if(ty==3) exe2='PERL C:\GPSUSER52\SCRIPT\AUTO52_S30B.PL '//yyc//' '//dyc//'1 '//apath(1:ii-1)//camp
      if(ty==3) exe3='PERL C:\GPSUSER52\SCRIPT\AUTO52_S30C.PL '//yyc//' '//dyc//'1 '//apath(1:ii-1)//camp
      call system(exe1)
      rty=1
      call rpcoord(yy,doy,dyc,n,fixsta,fixcvs,epo,apath,ty,camp,nnf,rty)
      call system(exe2)
      rty=2
      call rpcoord(yy,doy,dyc,n,fixsta,fixcvs,epo,apath,ty,camp,nnf,rty)
      call system(exe3)

      return
      end

c **********************************************************************
c *   subroutine rpcoord : replace the coordinates of constrained      *
c *                        stations for file "GPS_RES.CRD"            *
c **********************************************************************
      subroutine rpcoord(yy,doy,dyc,n,fixsta,fixcvs,epo,apath,oty,camp,nnf,rty)

      implicit none
      integer i,j,ii,n,yy,doy,stat,ty,oty,nnf,rty
      character dyc*3,apath*200,fixsta(n)*4,inf1*50,inf2*50,mo*100,l*100
      character camp*7
      real*8 fixcvs(n,9),epo(n),yr
      logical alive

      if(mod(yy,4)==0) yr=real(yy)+(real(doy-1))/366.
      if(mod(yy,4)/=0) yr=real(yy)+(real(doy-1))/365.

      !! find the input file *******************************************
      do ii=1,200
        if(apath(ii:ii)==' ') then
          exit
        else if(apath(ii:ii)=='/') then
          apath(ii:ii)='\'
        end if
      end do

      if(rty==1) then
        if(oty/=3) inf1=apath(1:ii-1)//'\STA\COD_'//dyc//'1.CRD'
        if(oty/=3) inf2=apath(1:ii-1)//'\STA\COD_'//dyc//'1_o.CRD'
        if(oty==3) inf1=apath(1:ii-1)//camp//'\STA\COD_'//dyc//'1.CRD'
        if(oty==3) inf2=apath(1:ii-1)//camp//'\STA\COD_'//dyc//'1_o.CRD'
        inquire(file=inf1,exist=alive)
        if(alive) then
          mo='ren '//inf1//'COD_'//dyc//'1_o.CRD'
          call system(mo)
        else
          return
        end if
      else if(rty==2) then
        if(oty/=3) inf1=apath(1:ii-1)//'\STA\GPS_RES.CRD'
        if(oty/=3) inf2=apath(1:ii-1)//'\STA\GPS_RES_o.CRD'
        if(oty==3) inf1=apath(1:ii-1)//camp//'\STA\GPS_RES.CRD'
        if(oty==3) inf2=apath(1:ii-1)//camp//'\STA\GPS_RES_o.CRD'
        inquire(file=inf1,exist=alive)
        if(alive) then
          mo='ren '//inf1//'GPS_RES_o.CRD'
          call system(mo)
        else
          return
        end if
      end if

      !! read the input file and replace the coordinates ***************
      open(10,file=inf1)
      open(11,file=inf2,status='old')
      read(11,'(a80)')l
      write(10,'(a80)')l
      read(11,'(a80)')l
      write(10,'(a80)')l
      read(11,'(a66)')l
      write(10,'(a66)')l
      read(11,*)
      write(10,*)
      read(11,'(a72)')l
      write(10,'(a72)')l
      read(11,*)
      write(10,*)
      stat=0
      do while(stat==0)
        read(11,'(a71)',iostat=stat)l
        if(stat/=0)exit
        ty=0
        do i=1,nnf
          if(l(6:9)==fixsta(i)) then
            ty=1
            write(10,'(a21,3f15.4,a5)')l(1:21),(fixcvs(i,j)+fixcvs(i,      j+3)*(yr-epo(i)),j=1,3),l(67:71)
            exit
          end if
        end do
        if(ty==0) write(10,'(a71)')l
      end do
      close(11,status='delete')
      close(10)

      return
      end

c **********************************************************************
c *   subroutine check : identify the quality of constrained stations  *
c *                      and choose the propor constrained stations    *
c **********************************************************************
CC     subroutine check(yy,mm,dd,doy,camp,apath,opath,spath,n,fixsta,
CC    +fixcvs,epo,nnf)
CC
CC     implicit none
CC     integer i,n,yy,mm,dd,doy,ty,ne,stat,checkty,nb,nnf,nn
CC     character fixsta(n)*4,camp*7,apath*200,opath*200,spath*200
CC     character line*100,chkfile*50
CC     character,allocatable::bsta(:)*4
CC     real*8 fixcvs(n,9),epo(n)
CC     logical alive
CC
CC     write(*,'(" Start to check the constrained station data ...",/)')
CC
CC     checkty=0
CC     nnf=n
CC     chkfile='C:\BERN52\AUTO\CHECK\'//camp//'.CHK'
CC     inquire(file=chkfile,exist=alive)
CC     if(alive) then
CC       open(11,file=chkfile,status='old')
CC       stat=0
CC       do while(stat==0)
CC         read(11,'(a)',iostat=stat)line
CC         if(stat/=0) exit
CC         if(line(29:31)==' ok') checkty=2
CC         if(line(29:31)=='bad') checkty=1
CC         if(line(1:25)=='Number of bad station(s):') then
CC           read(line,'(25x,i4)')nb
CC           if(nb>0) then
CC             allocate(bsta(nb))
CC             read(11,*)line,line,(bsta(i),i=1,nb)
CC             exit
CC           end if
CC         end if
CC       end do
CC       close(11)
CC     else
CC       nb=0
CC       call preBPE_check(yy,mm,dd,doy,n,camp,apath,opath,spath,fixsta,
CC    +  fixcvs,ty,ne,nnf)
CC       if(ty==1) then
CC         call del_camp(apath,camp)
CC         open(10,chkfile)
CC         write(10,'("sta1 sta2   dif    nor")')
CC         write(10,'("---------------------------")')
CC         write(10,'("!--------------------------  ok")')
CC         write(10,'("Number of bad station(s):   0")')
CC         close(10)
CC         return
CC       end if
CC       call exe_bat(yy,doy,camp,apath,3,n,fixsta,fixcvs,epo,nnf)
CC       call cmpbline(yy,doy,camp,apath,n,fixsta,fixcvs,epo,ne,nb,
CC    +  bsta,checkty,nnf)
CC       call del_camp(apath,camp)
CC       open(11,file=chkfile,status='old')
CC       stat=0
CC       do while(stat==0)
CC         read(11,'(a)',iostat=stat)line
CC         if(stat/=0) exit
CC         if(line(29:31)==' ok') checkty=2
CC         if(line(29:31)=='bad') checkty=1
CC         if(line(1:25)=='Number of bad station(s):') then
CC           read(line,'(25x,i4)')nb
CC           if(nb>0) then
CC             allocate(bsta(nb))
CC             read(11,*)line,line,(bsta(i),i=1,nb)
CC             exit
CC           end if
CC         end if
CC       end do
CC       close(11)
CC     end if
CC
CC     if(nb>0) call re_fix(nb,bsta,n,fixsta,fixcvs,epo,nnf)
CC     if(checkty==2.and.nb==0) return
CC     if(checkty==2.and.nb>0) then
CC       deallocate(bsta)
CC       return
CC     end if
CC
CC     call preBPE_check(yy,mm,dd,doy,n,camp,apath,opath,spath,fixsta,
CC    +fixcvs,ty,ne,nnf)
CC
CC     nn=0
CC     if(ty==1) then
CC       call del_camp(apath,camp)
CC       open(10,chkfile)
CC       write(10,'("sta1 sta2   dif    nor")')
CC       write(10,'("---------------------------")')
CC       write(10,'("!--------------------------  ok")')
CC       write(10,'("Number of bad station(s):   0")')
CC       close(10)
CC       return
CC     else if(ty==0) then
CC       do while(checkty==1)
CC         nn=nn+1
CC         if(nn/=1) then
CC           call preBPE_check(yy,mm,dd,doy,n,camp,apath,opath,spath,
CC    +      fixsta,fixcvs,ty,ne,nnf)
CC           open(11,file=chkfile,status='old')
CC           stat=0
CC           do while(stat==0)
CC             read(11,'(a)',iostat=stat)line
CC             if(stat/=0) exit
CC             if(line(1:25)=='Number of bad station(s):') then
CC               read(line,'(25x,i4)')nb
CC               if(nb>0) then
CC                 allocate(bsta(nb))
CC                 read(11,*)line,line,(bsta(i),i=1,nb)
CC                 exit
CC               end if
CC             end if
CC           end do
CC           close(11)
CC         end if
CC         call exe_bat(yy,doy,camp,apath,3,n,fixsta,fixcvs,epo,nnf)
CC         call cmpbline(yy,doy,camp,apath,n,fixsta,fixcvs,epo,ne,nb,
CC    +    bsta,checkty,nnf)
CC         call del_camp(apath,camp)
CC         if(nb>0) deallocate(bsta)
CC       end do
CC     end if
CC
CC     return
CC     end

c **********************************************************************
c *   subroutine preBPE : preparation for BPE                          *
c *     (1) create 11 folders, including 'ATM', 'BPE', 'GRD', 'MSC',   *
c *         'OBS', 'ORB', 'ORX', 'OUT', 'RAW', 'SOL', and 'STA'        *
c *     (2) copy GPS raw data to folder "RAW"                          *
c *     (3) copy 'C04_yyyy.ERP' and 'IGSWWWWD.SP3' to folder "ORB"     *
c *     (4) create a station file "RINEX.CRD" in the folder "STA"      *
c *     (5) create a session file "SESSIONS.SES" in the folder "STA"   *
c *     (6) create a fixed station file "FIXSTA.SIG" in the folder     *
c *         "STA"                                                      *
c **********************************************************************
      subroutine preBPE_check(yy,mm,dd,doy,nf,camp,oapath,oopath,ospath,fixsta,fixcvs,tty,n,nnf)

      implicit none
      integer i,ii,iii,j,yy,mm,dd,doy,nf,n,stat,k,ty,tyy,tty,nnf,y1
      integer day_plus,gpsweek,weekday
      character*200 apath,opath,spath,oapath,oopath,ospath
      character camp*7,tmp*100,fod(11)*3,osta(nf)*4,line*100,iweek*5
      character dyc*3,mmc*2,ddc*2,yyc*4,y1c*2,inf*50,fixsta(nf)*4
      real*8 fixcvs(nf,9)
      data fod /'ATM','BPE','GRD','MSC','OBS','ORB','ORX','OUT','RAW', 'SOL','STA'/

      tty=0
      apath=oapath
      opath=oopath
      spath=ospath
      call int2char(doy,dyc)
      if(yy> 1999) y1=yy-2000
      if(yy<=1999) y1=yy-1900
      call int2char3(y1,y1c)
      do i=1,nf
        osta(i)=''
      end do

      !! create folders ************************************************
      do ii=1,200
        if(apath(ii:ii)==' ') then
          apath(ii:ii+6)=camp
          exit
        end if
      end do
      do i=1,11
        tmp='mkdir '//apath(1:ii+6)//'\'//fod(i)
        call system(tmp)
      end do
      !! copy GPS raw data to folder "RAW" *****************************
      do iii=1,200
        if(opath(iii:iii)==' ') then
          exit
        end if
      end do
      do i=1,nnf
CC        tmp='copy/y '//opath(1:iii-1)//fixsta(i)//dyc//'1.'//y1c//'o '//
CC     +  apath(1:ii+6)//'\'//fod(9)
        call system(tmp)
      end do
      !! copy sp3 file to folder "ORB" *********************************
      day_plus=(yy-1995)*365+doy+int((yy-1993)/4)-1
      gpsweek=int(day_plus/7)+782
      weekday=mod(real(day_plus),7.)
      open(99,file='tmp')
      if(gpsweek<1000) write(99,'("0",i3,i1)') gpsweek,weekday
      if(gpsweek>=1000) write(99,'(i4,i1)') gpsweek,weekday
      rewind(99)
      read(99,'(a5)')iweek
      close(99,status='delete')
      do iii=1,200
        if(spath(iii:iii)==' ') then
          spath(iii:iii+12)='ig?'//iweek//'.sp3 '
          exit
        end if
      end do
      tmp='copy/y '//spath(1:iii+12)//apath(1:ii+6)//'\'//fod(6)
      call system(tmp)
      !! copy erp file to folder "ORB" *********************************
      call int2char2(yy,yyc)
      tmp='copy/y C:\BERN52\GPS\GEN\C04_'//yyc//'.ERP '//apath(1:ii+6)//'\'//fod(6)
      call system(tmp)
      tmp='copy/y C:\BERN52\AUTO\T97_2010.CRD '//apath(1:ii+6)//'\'//fod(11)
      call system(tmp)
CC      !! create "RINEX.CRD" ********************************************
CC      inf=apath(1:ii+6)//'\'//fod(11)//'\RINEX.CRD'
CC      open(10,file=inf)
CC      write(10,*)'COORDINATE FILE CREATED BY RXOBV3'
CC      write(10,*)'------------------------------------------------------
CC     +--------------------------'
CC      write(10,*)'LOCAL GEODETIC DATUM: ITRF2008          EPOCH:'
CC      write(10,*)
CC      write(10,*)'NUM  STATION NAME           X (M)          Y (M)
CC     +    Z (M)     FLAG'
CC      write(10,*)
CC      close(10)
      !! create "SESSIONS.SES" *****************************************
      inf=apath(1:ii+6)//'\'//fod(11)//'\SESSIONS.SES'
      call int2char3(mm,mmc)
      call int2char3(dd,ddc)
      open(10,file=inf)
      write(10,*)
      write(10,'("LIST_OF_SESSIONS 1  """,a3,"1"" """,i4," ",a2," ",a2,""" ""00 00 00"" """,i4," ",a2," ",a2,""" ""23 59 59""")')dyc,yy,mmc,ddc,yy,mmc,ddc
      write(10,*)'  ## widget = uniline; check_strlen.1 = 4; check_type.3 = time'
      write(10,*)'  ## check_type.5 = time'
      write(10,*)
      write(10,*)
      write(10,*)'# BEGIN_PANEL NO_CONDITION #####################################################'
      write(10,*)'# SESSION TABLE                         #'
      write(10,*)'#                         #'
      write(10,*)'#    SESSION                START EPOCH   END EPOCH             #'
      write(10,*)'#   IDENTIFIER         yyyy mm dd  hh mm ss        yyyy mm dd  hh mm ss        #'
      write(10,*)'#    > %%%%            %%%%%%%%%%  %%%%%%%%        %%%%%%%%%%  %%%%%%%% <      # LIST_OF_SESSIONS'
      write(10,*)'#                         #'
      write(10,*)'# END_PANEL ####################################################################'
      close(10)
CC      !! create "FIXSTA.SIG" *******************************************
CC      tmp='for %f in ('//apath(1:ii+6)//'\'//fod(11)//'\*.??o) do echo
CC     + %f >> auto52.inp'
CC      call system(tmp)
CC      n=0
CC      stat=0
CC      open(10,file='C:\BERN52\AUTO\auto52.inp')
CC      do i=1,nf
CC        read(10,'(a)',iostat=stat)line
CC        if(stat/=0) exit
CC        n=n+1
CC        do k=1,200
CC          if(line(k:k)==' ') then
CC            osta(n)=line(k-12:k-9)
CC            exit
CC          end if
CC        end do
CC      end do
CC      close(10,status='delete')
CC      if(n==1) then
CC        tty=1
CC        return
CC      end if
CC      inf=apath(1:ii+6)//'\'//fod(11)//'\FIXSTA.SIG'
CC      open(10,file=inf)
CC      write(10,*)'Station sigma file
CC     +           25-OCT-03 15:46'
CC      write(10,*)'------------------------------------------------------
CC     +--------------------------'
CC      write(10,*)
CC      write(10,*)'Station name            sigma1    sigma2    sigma3
CC     +                          '
CC      write(10,*)'****************       **.****   **.****   **.****
CC     +                          '
CC      ty=0
CC      do i=1,nnf
CC        tyy=0
CC        if(ty==0) then
CC          do j=1,n
CC            if(fixsta(i)==osta(j)) then
CC              write(10,'(a4,20x,"0.0001    0.0001    0.0001")')fixsta(i)
CC              ty=1
CC              tyy=1
CC              exit
CC            end if
CC          end do
CC        end if
CC        if(tyy==1) cycle
CC        write(10,'(a4,20x,"9.9999    9.9999    9.9999")')fixsta(i)
CC      end do
CC      close(10)

      return
      end

c **********************************************************************
c *   subroutine cmpbline : compare the calculated and theoretical     *
c *                         lengths of baselines between constrained   *
c *                         stations                                   *
c **********************************************************************
      subroutine cmpbline(yy,doy,camp,oapath,nf,fixsta,fixcvs,epo,ne,nbb,bbsta,checkty,nnf)

      implicit none
      integer i,j,k,ii,yy,doy,y,nf,ne,nc,nn,nb,s1,s2,ch,nbb,checkty,pf
      integer nnf
      integer,allocatable::ty(:)
      character chkfile*50,camp*7,apath*200,oapath*200,dyc*3,yc*2,inf*50
      character fixsta(nf)*4,sta(ne)*4,bsta(nf)*4,bbsta(nbb)*4
      character,allocatable::csta(:)*4
      real*8 yr,fixcvs(nf,9),epo(nf),cod1(nf,3),cod2(ne,3),blin1,blin2
      real*8 a,b,chk,dif1,dif2
      double precision,allocatable::dif(:)
      logical alive
      common pf
      apath=oapath
      if(pf==1) then
        a=2.0
        b=0.002
      else if(pf==2) then
        a=3.0
        b=0.03
      end if

      chkfile='C:\BERN52\AUTO\CHECK\'//camp//'.CHK'
      inquire(file=chkfile,exist=alive)
      open(11,file=chkfile)
      write(11,'("sta1 sta2   dif    nor")')
      write(11,'("---------------------------")')

      !! calculate theoretical coordinates
      if(mod(yy,4)==0) yr=real(yy)+(real(doy-1))/366.
      if(mod(yy,4)/=0) yr=real(yy)+(real(doy-1))/365.
      do i=1,nnf
        do j=1,3
          cod1(i,j)=(fixcvs(i,j)+fixcvs(i,j+3)*(yr-epo(i)))
        end do
      end do

      !! read final coordinate output file
      do ii=1,200
        if(apath(ii:ii)==' ') then
          apath(ii:ii+6)=camp
          exit
        end if
      end do
      if(yy<2000)  y=yy-1900
      if(yy>=2000) y=yy-2000
      call int2char(doy,dyc)
      call int2char3(y,yc)

CC      inf='1'
CC      inf=inf
      inf=apath(1:ii+6)//'\STA\FN'//yc//dyc//'1.CRD'
      open(10,file=inf,status='old')
        inf='1'
        inf=inf
        read(10,'(5/)')
        do i=1,ne
          read(10,'(5x,a4,12x,3f15.4)')sta(i),(cod2(i,j),j=1,3)
        end do
        close(10)

      !! check baselines
      nc=0
      do i=1,ne-1
        do j=i+1,ne
          nc=nc+1
        end do
      end do
      allocate(ty(nc),dif(nc),csta(nc*2))
      nc=0
      do i=1,ne-1
        do k=1,nnf
          if(fixsta(k)(1:4)==sta(i)(1:4)) then
            s1=k
            exit
          end if
        end do
        do j=i+1,ne
          do k=1,nnf
            if(fixsta(k)(1:4)==sta(j)(1:4)) then
              s2=k
              exit
            end if
          end do
          nc=nc+1
          blin1=sqrt((cod1(s1,1)-cod1(s2,1))**2+          (cod1(s1,2)-cod1(s2,2))**2+(cod1(s1,3)-cod1(s2,3))**2)
          blin2=sqrt((cod2(i,1)-cod2(j,1))**2+(cod2(i,2)-cod2(j,2))**2+          (cod2(i,3)-cod2(j,3))**2)
          dif(nc)=abs((blin1-blin2)*1.e3)
          chk=a+b*blin1*1.e-3
          csta(nc*2-1)=sta(i)
          csta(nc*2)=sta(j)
          if(dif(nc)<chk) then
           ty(nc)=0
           write(11,'(a4,"-",a4,2f7.2)')sta(i),sta(j),dif(nc),chk
          else
           ty(nc)=1
           write(11,'(a4,"-",a4,2f7.2," BAD")')sta(i),sta(j),dif(nc),chk
          end if
        end do
      end do

      !!  look for bad station(s)
      nn=0
      do i=1,nc
        if(ty(i)==1) nn=nn+1
      end do
      ch=0
      if(nc==1) then
        if(nn==0) ch=0
        if(nn==1) ch=1
      else if(nc/=1) then
        if(nn<nc-1) ch=0
        if(nn>=nc-1) ch=1
      end if
      if(ch==0) then
        write(11,'("!--------------------------  ok")')
        checkty=0
        nb=0
        do i=1,ne
          nn=0
          do j=1,nc
            if(ty(j)==0) cycle
            if(sta(i)(1:4)==csta(j*2-1)(1:4)) nn=nn+1
            if(sta(i)(1:4)==csta(j*2)(1:4)) nn=nn+1
          end do
          if(nn==(ne-1)) then
            nb=nb+1
            bsta(nb)=sta(i)
          end if
        end do
        if(alive) then
          do i=nb+1,nb+nbb
            bsta(i)=bbsta(i-nb)
          end do
          nb=nb+nbb
        end if
      else if(ch==1) then
        if(nn/=1) then
          write(11,'("!-------------------------- bad")')
          checkty=1
          call bubble_sort_n(csta,dif,nc)
          k=nc-ne+3
          do while(.true.)
            nb=0
            k=k-1
            do i=1,ne
              nn=0
              do j=nc,k,-1
                if(sta(i)(1:4)==csta(j*2-1)(1:4)) nn=nn+1
                if(sta(i)(1:4)==csta(j*2)(1:4)) nn=nn+1
              end do
              if(nn==(ne-1)) then
                nb=nb+1
                bsta(nb)=sta(i)
              end if
            end do
            if(nb/=0) exit
          end do
          if(alive) then
            do i=nb+1,nb+nbb
              bsta(i)=bbsta(i-nb)
            end do
            nb=nb+nbb
          end if
        else if(nn==1) then
          write(11,'("!--------------------------  ok")')
          checkty=0
          do i=1,ne
            do j=1,nnf
              if(fixsta(j)(1:4)==sta(i)(1:4)) then
                if(i==1) dif1=sqrt((cod1(j,1)-cod2(i,1))**2+(cod1(j,2)-          cod2(i,2))**2+(cod1(j,3)-cod1(i,3))**2)
                if(i==2) dif2=sqrt((cod1(j,1)-cod2(i,1))**2+(cod1(j,2)-          cod2(i,2))**2+(cod1(j,3)-cod1(i,3))**2)
                exit
              end if
            end do
          end do
          nb=1
          if(dif1>=dif2) bsta(1)(1:4)=sta(1)(1:4)
          if(dif2>dif1)  bsta(1)(1:4)=sta(2)(1:4)
          if(alive) then
            do i=nb+1,nb+nbb
              bsta(i)=bbsta(i-nb)
            end do
            nb=nb+nbb
          end if
        end if
      end if
      if(nb==0) then
        write(11,'("Number of bad station(s): ",i3)')nb
      else if(nb/=0) then
        write(11,'("Number of bad station(s): ",i3)')nb
        write(11,'("BAD station(s): ",10(a4,1x))')(bsta(i),i=1,nb)
      end if

      close(11)

      !! re-organize fixsta, fixcvs, epo
      if(ch==0.and.nb>0) call re_fix(nb,bsta,nf,fixsta,fixcvs,epo,nnf)
      if(ch==1.and.nn>=1.and.nb>0)call re_fix(nb,bsta,nf,fixsta,fixcvs,epo,nnf)

      deallocate(ty,dif,csta)

      return
      end

c **********************************************************************
c *   subroutine re_fix : re-organize the parameters of fixsta, fixcvs,*
c *                       and epo                                      *
c **********************************************************************
      subroutine re_fix(nb,bsta,nf,fixsta,fixcvs,epo,nnf)

      implicit none
      integer i,j,nnf,nb,nf,tty
      character fixsta(nf)*4,nfixsta(nf)*4,bsta(nb)*4
      real*8 fixcvs(nf,9),nfixcvs(nf,9),epo(nf),nepo(nf)

      nnf=0
      if(nb>0) then
        do i=1,nf
          tty=0
          do j=1,nb
            if(fixsta(i)==bsta(j)) then
              tty=1
              exit
            end if
          end do
          if(tty==1) cycle
          nnf=nnf+1
          nfixsta(nnf)=fixsta(i)
          if(i>1) then
            do j=1,i-1
              if(nfixsta(nnf)==fixsta(j)) then
                tty=1
                nnf=nnf-1
                exit
              end if
            end do
          end if
          if(tty==1) cycle
          nepo(nnf)=epo(i)
          do j=1,9
            nfixcvs(nnf,j)=fixcvs(i,j)
          end do
        end do
        do i=1,nnf
          fixsta(i)=nfixsta(i)
          epo(i)=nepo(i)
          do j=1,9
            fixcvs(i,j)=nfixcvs(i,j)
          end do
        end do
      end if

      return
      end

c **********************************************************************
c *   subroutine del_camp : delete all files and folders within the    *
c *                         campaign and itsself                       *
c **********************************************************************
      subroutine del_camp(oapath,camp)

      implicit none
      integer i
      character apath*200,oapath*200,tmp*200,camp*7

      apath=oapath
      do i=1,200
        if(apath(i:i)==' ') then
          apath(i:i+6)=camp
          exit
        end if
      end do
      tmp='RMDIR '//apath(1:i+6)//' /S /Q'
      call system(tmp)

      return
      end

c **********************************************************************
c *   subroutine bubble_sort_n : sorting method                        *
c **********************************************************************
      subroutine bubble_sort_n(a,b,n)

      implicit none
      integer n,i,j
      character a(n*2)*4,tmp1*4,tmp2*4
      real*8 b(n),tmp3

      do i=n-1,1,-1
        do j=1,i
          if(b(j).gt.b(j+1)) then
            tmp1=a(j*2-1)
            tmp2=a(j*2)
            tmp3=b(j)
            a(j*2-1)=a((j+1)*2-1)
            a(j*2)=a((j+1)*2)
            b(j)=b(j+1)
            a((j+1)*2-1)=tmp1
            a((j+1)*2)=tmp2
            b(j+1)=tmp3
          end if
        end do
      end do

      return
      end

c **********************************************************************
c *   subroutine int2char : change integer to character                *
c **********************************************************************
      subroutine int2char(a,b)
      integer a
      character b*3

      if (a==0)   b='000'
      if (a==1)   b='001'
      if (a==2)   b='002'
      if (a==3)   b='003'
      if (a==4)   b='004'
      if (a==5)   b='005'
      if (a==6)   b='006'
      if (a==7)   b='007'
      if (a==8)   b='008'
      if (a==9)   b='009'
      if (a==10)  b='010'
      if (a==11)  b='011'
      if (a==12)  b='012'
      if (a==13)  b='013'
      if (a==14)  b='014'
      if (a==15)  b='015'
      if (a==16)  b='016'
      if (a==17)  b='017'
      if (a==18)  b='018'
      if (a==19)  b='019'
      if (a==20)  b='020'
      if (a==21)  b='021'
      if (a==22)  b='022'
      if (a==23)  b='023'
      if (a==24)  b='024'
      if (a==25)  b='025'
      if (a==26)  b='026'
      if (a==27)  b='027'
      if (a==28)  b='028'
      if (a==29)  b='029'
      if (a==30)  b='030'
      if (a==31)  b='031'
      if (a==32)  b='032'
      if (a==33)  b='033'
      if (a==34)  b='034'
      if (a==35)  b='035'
      if (a==36)  b='036'
      if (a==37)  b='037'
      if (a==38)  b='038'
      if (a==39)  b='039'
      if (a==40)  b='040'
      if (a==41)  b='041'
      if (a==42)  b='042'
      if (a==43)  b='043'
      if (a==44)  b='044'
      if (a==45)  b='045'
      if (a==46)  b='046'
      if (a==47)  b='047'
      if (a==48)  b='048'
      if (a==49)  b='049'
      if (a==50)  b='050'
      if (a==51)  b='051'
      if (a==52)  b='052'
      if (a==53)  b='053'
      if (a==54)  b='054'
      if (a==55)  b='055'
      if (a==56)  b='056'
      if (a==57)  b='057'
      if (a==58)  b='058'
      if (a==59)  b='059'
      if (a==60)  b='060'
      if (a==61)  b='061'
      if (a==62)  b='062'
      if (a==63)  b='063'
      if (a==64)  b='064'
      if (a==65)  b='065'
      if (a==66)  b='066'
      if (a==67)  b='067'
      if (a==68)  b='068'
      if (a==69)  b='069'
      if (a==70)  b='070'
      if (a==71)  b='071'
      if (a==72)  b='072'
      if (a==73)  b='073'
      if (a==74)  b='074'
      if (a==75)  b='075'
      if (a==76)  b='076'
      if (a==77)  b='077'
      if (a==78)  b='078'
      if (a==79)  b='079'
      if (a==80)  b='080'
      if (a==81)  b='081'
      if (a==82)  b='082'
      if (a==83)  b='083'
      if (a==84)  b='084'
      if (a==85)  b='085'
      if (a==86)  b='086'
      if (a==87)  b='087'
      if (a==88)  b='088'
      if (a==89)  b='089'
      if (a==90)  b='090'
      if (a==91)  b='091'
      if (a==92)  b='092'
      if (a==93)  b='093'
      if (a==94)  b='094'
      if (a==95)  b='095'
      if (a==96)  b='096'
      if (a==97)  b='097'
      if (a==98)  b='098'
      if (a==99)  b='099'
      if (a==100) b='100'
      if (a==101) b='101'
      if (a==102) b='102'
      if (a==103) b='103'
      if (a==104) b='104'
      if (a==105) b='105'
      if (a==106) b='106'
      if (a==107) b='107'
      if (a==108) b='108'
      if (a==109) b='109'
      if (a==110) b='110'
      if (a==111) b='111'
      if (a==112) b='112'
      if (a==113) b='113'
      if (a==114) b='114'
      if (a==115) b='115'
      if (a==116) b='116'
      if (a==117) b='117'
      if (a==118) b='118'
      if (a==119) b='119'
      if (a==120) b='120'
      if (a==121) b='121'
      if (a==122) b='122'
      if (a==123) b='123'
      if (a==124) b='124'
      if (a==125) b='125'
      if (a==126) b='126'
      if (a==127) b='127'
      if (a==128) b='128'
      if (a==129) b='129'
      if (a==130) b='130'
      if (a==131) b='131'
      if (a==132) b='132'
      if (a==133) b='133'
      if (a==134) b='134'
      if (a==135) b='135'
      if (a==136) b='136'
      if (a==137) b='137'
      if (a==138) b='138'
      if (a==139) b='139'
      if (a==140) b='140'
      if (a==141) b='141'
      if (a==142) b='142'
      if (a==143) b='143'
      if (a==144) b='144'
      if (a==145) b='145'
      if (a==146) b='146'
      if (a==147) b='147'
      if (a==148) b='148'
      if (a==149) b='149'
      if (a==150) b='150'
      if (a==151) b='151'
      if (a==152) b='152'
      if (a==153) b='153'
      if (a==154) b='154'
      if (a==155) b='155'
      if (a==156) b='156'
      if (a==157) b='157'
      if (a==158) b='158'
      if (a==159) b='159'
      if (a==160) b='160'
      if (a==161) b='161'
      if (a==162) b='162'
      if (a==163) b='163'
      if (a==164) b='164'
      if (a==165) b='165'
      if (a==166) b='166'
      if (a==167) b='167'
      if (a==168) b='168'
      if (a==169) b='169'
      if (a==170) b='170'
      if (a==171) b='171'
      if (a==172) b='172'
      if (a==173) b='173'
      if (a==174) b='174'
      if (a==175) b='175'
      if (a==176) b='176'
      if (a==177) b='177'
      if (a==178) b='178'
      if (a==179) b='179'
      if (a==180) b='180'
      if (a==181) b='181'
      if (a==182) b='182'
      if (a==183) b='183'
      if (a==184) b='184'
      if (a==185) b='185'
      if (a==186) b='186'
      if (a==187) b='187'
      if (a==188) b='188'
      if (a==189) b='189'
      if (a==190) b='190'
      if (a==191) b='191'
      if (a==192) b='192'
      if (a==193) b='193'
      if (a==194) b='194'
      if (a==195) b='195'
      if (a==196) b='196'
      if (a==197) b='197'
      if (a==198) b='198'
      if (a==199) b='199'
      if (a==200) b='200'
      if (a==201) b='201'
      if (a==202) b='202'
      if (a==203) b='203'
      if (a==204) b='204'
      if (a==205) b='205'
      if (a==206) b='206'
      if (a==207) b='207'
      if (a==208) b='208'
      if (a==209) b='209'
      if (a==210) b='210'
      if (a==211) b='211'
      if (a==212) b='212'
      if (a==213) b='213'
      if (a==214) b='214'
      if (a==215) b='215'
      if (a==216) b='216'
      if (a==217) b='217'
      if (a==218) b='218'
      if (a==219) b='219'
      if (a==220) b='220'
      if (a==221) b='221'
      if (a==222) b='222'
      if (a==223) b='223'
      if (a==224) b='224'
      if (a==225) b='225'
      if (a==226) b='226'
      if (a==227) b='227'
      if (a==228) b='228'
      if (a==229) b='229'
      if (a==230) b='230'
      if (a==231) b='231'
      if (a==232) b='232'
      if (a==233) b='233'
      if (a==234) b='234'
      if (a==235) b='235'
      if (a==236) b='236'
      if (a==237) b='237'
      if (a==238) b='238'
      if (a==239) b='239'
      if (a==240) b='240'
      if (a==241) b='241'
      if (a==242) b='242'
      if (a==243) b='243'
      if (a==244) b='244'
      if (a==245) b='245'
      if (a==246) b='246'
      if (a==247) b='247'
      if (a==248) b='248'
      if (a==249) b='249'
      if (a==250) b='250'
      if (a==251) b='251'
      if (a==252) b='252'
      if (a==253) b='253'
      if (a==254) b='254'
      if (a==255) b='255'
      if (a==256) b='256'
      if (a==257) b='257'
      if (a==258) b='258'
      if (a==259) b='259'
      if (a==260) b='260'
      if (a==261) b='261'
      if (a==262) b='262'
      if (a==263) b='263'
      if (a==264) b='264'
      if (a==265) b='265'
      if (a==266) b='266'
      if (a==267) b='267'
      if (a==268) b='268'
      if (a==269) b='269'
      if (a==270) b='270'
      if (a==271) b='271'
      if (a==272) b='272'
      if (a==273) b='273'
      if (a==274) b='274'
      if (a==275) b='275'
      if (a==276) b='276'
      if (a==277) b='277'
      if (a==278) b='278'
      if (a==279) b='279'
      if (a==280) b='280'
      if (a==281) b='281'
      if (a==282) b='282'
      if (a==283) b='283'
      if (a==284) b='284'
      if (a==285) b='285'
      if (a==286) b='286'
      if (a==287) b='287'
      if (a==288) b='288'
      if (a==289) b='289'
      if (a==290) b='290'
      if (a==291) b='291'
      if (a==292) b='292'
      if (a==293) b='293'
      if (a==294) b='294'
      if (a==295) b='295'
      if (a==296) b='296'
      if (a==297) b='297'
      if (a==298) b='298'
      if (a==299) b='299'
      if (a==300) b='300'
      if (a==301) b='301'
      if (a==302) b='302'
      if (a==303) b='303'
      if (a==304) b='304'
      if (a==305) b='305'
      if (a==306) b='306'
      if (a==307) b='307'
      if (a==308) b='308'
      if (a==309) b='309'
      if (a==310) b='310'
      if (a==311) b='311'
      if (a==312) b='312'
      if (a==313) b='313'
      if (a==314) b='314'
      if (a==315) b='315'
      if (a==316) b='316'
      if (a==317) b='317'
      if (a==318) b='318'
      if (a==319) b='319'
      if (a==320) b='320'
      if (a==321) b='321'
      if (a==322) b='322'
      if (a==323) b='323'
      if (a==324) b='324'
      if (a==325) b='325'
      if (a==326) b='326'
      if (a==327) b='327'
      if (a==328) b='328'
      if (a==329) b='329'
      if (a==330) b='330'
      if (a==331) b='331'
      if (a==332) b='332'
      if (a==333) b='333'
      if (a==334) b='334'
      if (a==335) b='335'
      if (a==336) b='336'
      if (a==337) b='337'
      if (a==338) b='338'
      if (a==339) b='339'
      if (a==340) b='340'
      if (a==341) b='341'
      if (a==342) b='342'
      if (a==343) b='343'
      if (a==344) b='344'
      if (a==345) b='345'
      if (a==346) b='346'
      if (a==347) b='347'
      if (a==348) b='348'
      if (a==349) b='349'
      if (a==350) b='350'
      if (a==351) b='351'
      if (a==352) b='352'
      if (a==353) b='353'
      if (a==354) b='354'
      if (a==355) b='355'
      if (a==356) b='356'
      if (a==357) b='357'
      if (a==358) b='358'
      if (a==359) b='359'
      if (a==360) b='360'
      if (a==361) b='361'
      if (a==362) b='362'
      if (a==363) b='363'
      if (a==364) b='364'
      if (a==365) b='365'
      if (a==366) b='366'
      if (a==367) b='367'
      if (a==368) b='368'
      if (a==369) b='369'
      if (a==370) b='370'
      if (a==371) b='371'
      if (a==372) b='372'
      if (a==373) b='373'
      if (a==374) b='374'
      if (a==375) b='375'
      if (a==376) b='376'
      if (a==377) b='377'
      if (a==378) b='378'
      if (a==379) b='379'
      if (a==380) b='380'
      if (a==381) b='381'
      if (a==382) b='382'
      if (a==383) b='383'
      if (a==384) b='384'
      if (a==385) b='385'
      if (a==386) b='386'
      if (a==387) b='387'
      if (a==388) b='388'
      if (a==389) b='389'
      if (a==390) b='390'
      if (a==391) b='391'
      if (a==392) b='392'
      if (a==393) b='393'
      if (a==394) b='394'
      if (a==395) b='395'
      if (a==396) b='396'
      if (a==397) b='397'
      if (a==398) b='398'
      if (a==399) b='399'
      return
      end

c **********************************************************************
c *   subroutine int2char2 : change integer to character                *
c **********************************************************************
      subroutine int2char2(a,b)
      integer a
      character b*4

      if (a==1990)   b='1990'
      if (a==1991)   b='1991'
      if (a==1992)   b='1992'
      if (a==1993)   b='1993'
      if (a==1994)   b='1994'
      if (a==1995)   b='1995'
      if (a==1996)   b='1996'
      if (a==1997)   b='1997'
      if (a==1998)   b='1998'
      if (a==1999)   b='1999'
      if (a==2000)   b='2000'
      if (a==2001)   b='2001'
      if (a==2002)   b='2002'
      if (a==2003)   b='2003'
      if (a==2004)   b='2004'
      if (a==2005)   b='2005'
      if (a==2006)   b='2006'
      if (a==2007)   b='2007'
      if (a==2008)   b='2008'
      if (a==2009)   b='2009'
      if (a==2010)   b='2010'
      if (a==2011)   b='2011'
      if (a==2012)   b='2012'
      if (a==2013)   b='2013'
      if (a==2014)   b='2014'
      if (a==2015)   b='2015'
      if (a==2016)   b='2016'
      if (a==2017)   b='2017'
      if (a==2018)   b='2018'
      if (a==2019)   b='2019'
      if (a==2020)   b='2020'
      if (a==2021)   b='2021'
      if (a==2022)   b='2022'
      if (a==2023)   b='2023'
      if (a==2024)   b='2024'
      if (a==2025)   b='2025'
      if (a==2026)   b='2026'
      if (a==2027)   b='2027'
      if (a==2028)   b='2028'
      if (a==2029)   b='2029'
      return
      end

c **********************************************************************
c *   subroutine int2char3 : change integer to character                *
c **********************************************************************
      subroutine int2char3(a,b)
      integer a
      character b*2

      if (a==0)   b='00'
      if (a==1)   b='01'
      if (a==2)   b='02'
      if (a==3)   b='03'
      if (a==4)   b='04'
      if (a==5)   b='05'
      if (a==6)   b='06'
      if (a==7)   b='07'
      if (a==8)   b='08'
      if (a==9)   b='09'
      if (a==10)  b='10'
      if (a==11)  b='11'
      if (a==12)  b='12'
      if (a==13)  b='13'
      if (a==14)  b='14'
      if (a==15)  b='15'
      if (a==16)  b='16'
      if (a==17)  b='17'
      if (a==18)  b='18'
      if (a==19)  b='19'
      if (a==20)  b='20'
      if (a==21)  b='21'
      if (a==22)  b='22'
      if (a==23)  b='23'
      if (a==24)  b='24'
      if (a==25)  b='25'
      if (a==26)  b='26'
      if (a==27)  b='27'
      if (a==28)  b='28'
      if (a==29)  b='29'
      if (a==30)  b='30'
      if (a==31)  b='31'
      if (a==32)  b='32'
      if (a==33)  b='33'
      if (a==34)  b='34'
      if (a==35)  b='35'
      if (a==36)  b='36'
      if (a==37)  b='37'
      if (a==38)  b='38'
      if (a==39)  b='39'
      if (a==40)  b='40'
      if (a==41)  b='41'
      if (a==42)  b='42'
      if (a==43)  b='43'
      if (a==44)  b='44'
      if (a==45)  b='45'
      if (a==46)  b='46'
      if (a==47)  b='47'
      if (a==48)  b='48'
      if (a==49)  b='49'
      if (a==50)  b='50'
      if (a==51)  b='51'
      if (a==52)  b='52'
      if (a==53)  b='53'
      if (a==54)  b='54'
      if (a==55)  b='55'
      if (a==56)  b='56'
      if (a==57)  b='57'
      if (a==58)  b='58'
      if (a==59)  b='59'
      if (a==60)  b='60'
      if (a==61)  b='61'
      if (a==62)  b='62'
      if (a==63)  b='63'
      if (a==64)  b='64'
      if (a==65)  b='65'
      if (a==66)  b='66'
      if (a==67)  b='67'
      if (a==68)  b='68'
      if (a==69)  b='69'
      if (a==70)  b='70'
      if (a==71)  b='71'
      if (a==72)  b='72'
      if (a==73)  b='73'
      if (a==74)  b='74'
      if (a==75)  b='75'
      if (a==76)  b='76'
      if (a==77)  b='77'
      if (a==78)  b='78'
      if (a==79)  b='79'
      if (a==80)  b='80'
      if (a==81)  b='81'
      if (a==82)  b='82'
      if (a==83)  b='83'
      if (a==84)  b='84'
      if (a==85)  b='85'
      if (a==86)  b='86'
      if (a==87)  b='87'
      if (a==88)  b='88'
      if (a==89)  b='89'
      if (a==90)  b='90'
      if (a==91)  b='91'
      if (a==92)  b='92'
      if (a==93)  b='93'
      if (a==94)  b='94'
      if (a==95)  b='95'
      if (a==96)  b='96'
      if (a==97)  b='97'
      if (a==98)  b='98'
      if (a==99)  b='99'
      return
      end


      subroutine getarg_local(numargs,arg,comline)
      implicit real*8(a-h,o-z)
      character  arg(*)*20,comline*(*)

      n=len_trim(comline)
      n=n+1
      j1=0
      j2=0
      numargs=0

 560  ibegin=j2+1
      do i=ibegin, n
         if (comline(i:i).ne.' ') then
            j1=i
            do k=i,n
               if (comline(k:k).eq.' ') then
                  j2=k-1
                  numargs=numargs+1
                  arg(numargs)=comline(j1:j2)
                  go to 560
               endif
            end do
         endif
      end do
      return
      end
