      program     rdsinex
c     generate summary file from SINEX V1.0 solution output file
*----------------------------------------------------------------------------
* SOLUTION INDEPENDENT EXCHANGE FORMAT (SINEX) FOR SPACE GEODESY.
* - SINEX VERSION 1.00
* - FILE CREATED BY PROGRAM ADDNEQ V4.2
c     SNX 1.00 COD
c     2000.07.05

C     TRANSFER Bern50 Sinex file to Sum file
C     modified by dyj 2006/07/06

      implicit    none
      integer*2   maxsta
      parameter   (maxsta=50)
      integer*2   i,j,k,nargs,inbase,inincl,outbase,imax,ii,jj,
     .            ibase,nten,ista,outdump,doy,session
      real*8      x84(maxsta),y84(maxsta),z84(maxsta),
     .            dx,dy,dz,v11,v12,v22,v13,v23,v33
      real*8      rms,vcv(3*maxsta,3*maxsta),real(3)                 !,q(6,6)
      real*8      stdx(3*maxsta)
      character   insinex*40,outgps*40,line*120,baseline(28)*79,dump*40
      character*4 stanam(maxsta)
c        integer     index

      if (nargs.lt.2) then
         print *,' usage   : rdsinex insinex '
         print *,' insinex : input SINEX Solution text file'
           print *,' sinex file name format = yyyyddds.snx '
           print *,' yyyy=year; ddd=day of year; s=session '
         stop
      endif

      call getarg_local(1,insinex,i)

      call fnunit(inbase)
      call ofilopn1(inbase,insinex)
c     add by dyj at 2005/02/04
      i=index(insinex,'.')
      outgps=insinex(1:i)
        outgps(i+1:i+4)='sum'
c        write(*,*)outgps

c     add by dyj at 2006/07/06
      if (i.eq.9)then
          read(insinex,'(4x,i3,i1)')doy,session
        else
           print *,' sinex file name format = yyyyddds.snx '
           print *,' yyyy=year; ddd=day of year; s=session '
         stop
      endif



c        outgps='rdsinex.out'
      dump='rdsinex.dmp'
      call fnunit(outbase)
      open (outbase,file=outgps)
      call fnunit(outdump)
      open (outdump,file=dump)

c     read in a templete from a sample Summary
      call fnunit(inincl)
      open (inincl,file='rdtrim.dat',status='old')

      i=0
      do while(.true.)
         read(inincl,'(a)')line
         i=i+1
         baseline(i)(1:79)=line(1:79)
         if(line(1:7).eq.'#EOFSUM') exit
      enddo
      write(outdump,'(a)')' after read in template'
      write(*,'(a)')' after read in template'

c     read in SINEX file
      read(inbase,'(a)') line
      if(line(1:5).ne.'%=SNX')stop ' not sinex file'

C      do while(.true.)
C         read(inbase,'(a)',end=99999) line
C         if(line(1:22).eq.'*   RMS OF UNIT WEIGHT') EXIT
C      enddo
C      read(line(24:31),'(f8.0)') rms
C      write(outdump,'(a,f20.6)') 'rms=',rms
C      write(*,'(a,f20.6)') 'rms=',rms
C      if(rms .gt. 0.01) Print *,' warning ****  rms> 0.01 '

C      do while(.true.)
C         read(inbase,'(a)') line
C         if(line(1:12).eq.'+INPUT/FILES') exit
C      enddo
C     read(inbase,'(a)') line
C      read(inbase,'(a)') line
C      read(inbase,'(a)') line
      write(baseline(2),500)doy,session,doy,session
500   format('E:\YU\',i3,i1,'\',i3,i1)
      do k=1,3
           if(baseline(2)(6+k:6+k).eq.' ')then
              baseline(2)(6+k:6+k)='0'
              baseline(2)(11+k:11+k)='0'
           endif
      enddo



C      write(*,*)baseline(2)


c     read in station coordinates from SINEX
      do while(.true.)
         read(inbase,'(a)',end=99999) line
         if(line(1:18).eq.'+SOLUTION/ESTIMATE') EXIT
      enddo
      read(inbase,'(a)') line
      write(outdump,'(a)') ' up to solution estimates header'
      write(*,'(a)') ' up to solution estimates header'
      i=1
      do while(.true.)
         read(inbase,'(a)',end=99999) line
         if(line(1:1).eq.'-') exit
         write(outdump,'(a)') line
         write(*,'(a)') line
         read(line,'(14x,a4,28x,E22.15,e12.5)')stanam(i),x84(i),stdx(3*i-2)
         read(inbase,'(a)',end=99999) line
         read(line,'(46x,E22.15,e12.5)')y84(i),stdx(3*i-1)
         read(inbase,'(a)',end=99999) line
         read(line,'(46x,E22.15,e12.5)')z84(i),stdx(3*i)
         i=i+1
      enddo
      ista=i-1
      write(outdump,'(a,i5)') ' total station number=',ista
      write(*,'(a,i5)') ' total station number=',ista


c     read in station coordinates VCV matrix from SINEX
      do while(.true.)
         read(inbase,'(a)',end=99999) line
         if(line(1:33).eq.'+SOLUTION/MATRIX_ESTIMATE L COVA') EXIT
      enddo
      read(inbase,'(a)') line
c     read in lower triangulat matrix elements
      do while(.true.)
         read(inbase,'(a)') line
         if(line(1:1) .eq.'-') exit
         read(line,'(2i6,3E22.15)')i,k,real(1),real(2),real(3)
         do j=1,3
            if(k+j-1.gt.i) cycle
            vcv(i,k+j-1)=real(j)
         enddo
      enddo



99999 continue
      write(outdump,'(a,i6)')' total estimates coordinates =',i
      write(*,'(a,i6)')' total estimates coordinates number=',i
      if(i .ne. 3*ista) stop ' estimates number not correct !!!!'

c     fulfil square matrix elements
      imax=i
      do i=1,imax
         write(outdump,'(i5,2f10.4)')i,dsqrt(vcv(i,i)),stdx(i)
         do j=i+1,imax
            k=j
            vcv(i,j)=vcv(k,i)
         enddo
      enddo


c Start big loop
      ibase=0
      do i=1,ista
         do j=i+1,ista
            ibase=ibase+1
            do while(nten.ge.1)
               write(outdump,"('+',a,i7,a,i5)") 'record:  ',ibase
               write(*,"('+',a,i7,a,i5)") 'record:  ',ibase
               nten = 0
            end do
            nten = nten + 1
            write(outdump,*) ' baseline= ',ibase
            write(*,*) ' baseline= ',ibase
            baseline(1)(44:53)='SINEX V1.0'
C            baseline(2)(23:23)='\'
            dx=x84(j)-x84(i)
            dy=y84(j)-y84(i)
            dz=z84(j)-z84(i)
c            q(1,1)=vcv(3*i,3*i)
c            q(2,1)=vcv(3*i+1,3*i)
c            q(3,1)=vcv(3*i+2,3*i)
c            q(2,2)=vcv(3*i+1,3*i+1)
c            q(2,3)=vcv(3*i+1,3*i+2)
c            q(3,3)=vcv(3*i+2,3*i+2)
c            q(4,4)=vcv(3*j,3*j)
c            q(5,4)=vcv(3*j+1,3*j)
c            q(6,4)=vcv(3*j+2,3*j)
c            q(5,5)=vcv(3*j+1,3*j+1)
c            q(5,6)=vcv(3*j+1,3*j+2)
c            q(6,6)=vcv(3*j+2,3*j+2)
c            q(4,1)=vcv(3*j,3*i)
c            q(5,1)=vcv(3*j+1,3*i)
c            q(6,1)=vcv(3*j+2,3*i)
c            q(4,2)=vcv(3*j,3*i+1)
c            q(5,2)=vcv(3*j+1,3*i+1)
c            q(6,2)=vcv(3*j+2,3*i+1)
c            q(4,3)=vcv(3*j,3*i+2)
c            q(5,3)=vcv(3*j+1,3*i+2)
c            q(6,3)=vcv(3*j+2,3*i+2)
c            do k=1,6
c               do l=k+1,6
c                  q(k,l)=q(l,k)
c               enddo
c            enddo
            ii=3*i-2
            jj=3*j-2
            v11=vcv(ii,ii)+vcv(jj,jj)-2.d0*vcv(ii,jj)
            v22=vcv(ii+1,ii+1)+vcv(jj+1,jj+1)-2.d0*vcv(ii+1,jj+1)
            v33=vcv(ii+2,ii+2)+vcv(jj+2,jj+2)-2.d0*vcv(ii+2,jj+2)
            v12=vcv(ii,ii+1)+vcv(jj,jj+1)-vcv(ii,jj+1)-vcv(ii+1,jj)
            v13=vcv(ii,ii+2)+vcv(jj,jj+2)-vcv(ii,jj+2)-vcv(ii+2,jj)
            v23=vcv(ii+1,ii+2)+vcv(jj+1,jj+2)-vcv(ii+1,jj+2)
     .                                       -vcv(ii+2,jj+1)
c            v11=rms**2*v11
c            v22=rms**2*v22
c            v33=rms**2*v33
c            v12=rms**2*v12
c            v13=rms**2*v13
c            v23=rms**2*v23
            baseline(6)(13:16)=stanam(i)(1:4)
            baseline(12)(13:16)=stanam(j)(1:4)
            write(baseline(2)(16:23),'(i4,a4)') ibase,'.SSF'
              do k=1,4
                   if(baseline(2)(15+k:15+k).eq.' ')baseline(2)(15+k:15+k)='0'
              enddo
            write(baseline(7)(20:79),'(3f20.3)')x84(i),y84(i),z84(i)
            write(baseline(13)(20:79),'(3f20.3)')x84(j),y84(j),z84(j)
            write(baseline(20)(20:79),'(3f20.3)') dx,dy,dz
            write(baseline(21)(8:28),'(d21.13)') v11
            write(baseline(22)(8:49),'(2d21.13)')v12,v22
            write(baseline(23)(8:70),'(3d21.13)')v13,v23,v33

            write(outbase,'(a)') baseline


         enddo
      enddo


      stop 'normal'
      end


      subroutine fnunit(nu)
c     new version with nu integer*2
c     function :find free unit number    ;code come  from H.J.Euler in OSU
c     Declaration: nu   integer*2   free unit number
c                  error: nu = -1    probably no free unit number available
      implicit    none
      integer*2   nu, i
      logical     lopen

      lopen = .true.
      nu = 9
      do while (lopen)
         nu = nu + 1
         inquire(unit=nu,opened=lopen)
      end do
      open(unit=nu,file='junk##.$$',iostat=i)
      if (i .ne. 0) then
         nu = -1
      else
         close(unit=nu,status='delete')
      endif
      return
      end
      subroutine ofilopn1(iunit,oldname)
c-------------------------------------------c
c  open an old file,no read filname         c
c-------------------------------------------c
      character oldname*30
      logical logic
1     continue
ccc      read(*,'(a)')oldname
      inquire(file=oldname,exist=logic)
      if(.not.logic) then
          write(*,*)' file not exist, please reenter =?'
          read(*,'(a)')oldname
          go to 1
      else
          open(iunit,file=oldname,status='old')
      endif
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
