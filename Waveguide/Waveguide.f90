!  Waveguide.f90 
!
!  FUNCTIONS:
!  Waveguide - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Waveguide
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program Waveguide

    use global
    implicit none

    !! Variables
    complex*16, allocatable:: Rd(:), u_res(:)
    complex*16, external:: UI
    real*8, allocatable :: dzeta(:)
    real*8 :: t1, t2, t3, t4, tm, tp, eps, gr, pr, hx
    integer :: Np, i, j
    
    namelist/inp_values /c,f,h,Nx,x0,xn,Nz,z0,zn,eps,gr,pr  
    
    open(unit=4,file='inp.dat',status='old')
    
    read(4,inp_values)
        
    allocate(Rd(Nx), u_res(Nx), x(Nx))
              
    kappa=2*pi*f/c; !волновое число
    
!   инициализация x   
    hx = (xn-x0)/Nx
    do i = 1, Nx
        x(i) = x0 + i*hx;   print *, 'x ', x(i);
    enddo
        
!   вычисление полюсов 
    Np = int(kappa*h/pi + 0.5d0)
    allocate(dzeta(Np))
    t1 = 0
    t2 = 0
    do i = 1, Np
        dzeta(i) = sqrt(kappa*kappa-an(i)*an(i))
        print *, 'dzeta ', dzeta(i)
    enddo        
    
    ! [t1,t2],[t3,t4] - участки отклонения контура вниз (real*8)
    ! [t2,t3] - участок отклонения контура вверх (real*8)
    t1 = dzeta(Np)/2d0;  t2 = dzeta(1)+1d0;                                      
    t3=t2; t4=t3; tm=1d-2; tp=0d0;   print *, 't1, t2, tm, Np:', t1, t2, tm, Np; 
    
    !вычисление интеграла с помощью DINN5. Результаты записываются в Rd.
    call DINN5(UI,t1,t2,t3,t4,tm,tp,eps,pr,gr,Nx,Rd)
    
    !вычисление интеграла с помощью вычетов. Результаты записываются в u_res.
    do i = 1, Nx
        u_res(i)=0d0;
        do j = 1, Np
            u_res(i) = u_res(i) + res(z0,dzeta(j),j) * exp(-cci*dzeta(j)*x(i))
        enddo
        u_res(i) = -cci * u_res(i);
    enddo
    
    !запись данных в файлы
    open(unit = 1, file = 'UI/x.txt', status = 'unknown')
    open(unit = 2, file = 'UI/u.txt', status = 'unknown')
    open(unit = 3, file = 'UI/u_res.txt', status = 'unknown')    
    
    print *, 'x, |Rd|, |u_res|: '
    do i = 1, Nx
        !вывод в консоль
        print *, x(i), real(Rd(i)), real(u_res(i));
        !print *, x(i), real(Rd(i)), imag(Rd(i)), abs(Rd(i))
        print *
        !вывод в файлы
        Write(1,1) x(i)
        !Write(2,1) abs(Rd(i))
        !Write(3,1) abs(u_res(i))  
        Write(2,1) real(Rd(i))
        Write(3,1) real(u_res(i))
    enddo
    
    deallocate(Rd, x, u_res, dzeta) 
1   format(f10.5)
2   format(f10.5,2x,f20.4,2x,f20.15)     
    pause
    
    
    contains
    
!   sigma(dzeta_n)
    complex*16 function sig_n(n)
        integer :: n
        sig_n = -cci*an(n);        
    end function
    
!   производная от sigma(dzeta_n)
    complex*16 function sig_pr_n(dzt,n)
        real*8 :: dzt
        integer :: n
        sig_pr_n = dzt/sig_n(n);        
    end function
    
!   числитель K
    complex*16 function sh_n(z,n)
        real*8 :: z
        integer :: n
        sh_n = cci * sin(-an(n)*(z+h))
    end function
    
!   вычеты
    complex*16 function res(z,dzt,n)
        real*8 :: z,dzt
        integer :: n
        res = sh_n(z,n) / (dzt*sh_n(0d0,n)*h)        
    end function
    
    end program Waveguide

