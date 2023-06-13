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
    complex*16, allocatable:: Rd(:), u_res(:), Uom(:)
    complex*16, external:: K, UI, UIomega
    real*8, allocatable :: a(:), dzeta(:)
    real*8 :: t1, t2, t3, t4, tm, tp, eps, gr, pr
    integer :: Nx, Nz, Np, Nom, i, j
    real*8 :: an
        
    Nx=50; 
    allocate(Rd(Nx))
    allocate(u_res(Nx))
    allocate(x(Nx))
    !allocate(z(Nz))
    
    !начальные данные
    c=1d0 !фазовая скорость
    f=3d0 !частота
    h=1d0 !ширина слоя 
    kappa=2*pi*f/c; !волновое число
    
    !инициализация x и z          
    do i = 1, Nx
        x(i) = real(i)/30;
        print *, 'x ', x(i);
    enddo
    zj=-0.5d0;
    
    !нахождение t1 и t2.   t1 = dzeta1 / 2; t2 - максимальный из вещ-ных полюсов +1
    an=pi/2/h;
    Np=0; !кол-во вещ-ных полюсов
    if (kappa>an) then
        t1=sqrt(kappa*kappa-an*an);
        i=1;
        t2=t1;
        do while (kappa>an .AND. i<=100)        
            if(t2<sqrt(kappa*kappa-an*an)) then
                t2=sqrt(kappa*kappa-an*an);
            endif
            i=i+1;
            an=pi*(real(i)-1/2)/h;
        enddo
        t1=t1/2;
        t2=t2+1;
        Np=i;        
    else
        t1=0d0;
        t2=0d0;
    endif;
    print *, 't1, t2, Np:', t1, t2, Np, t1*2;
    
    !вычисление полюсов dzeta
    allocate(dzeta(Np));
    do i = 1, Np
        an=pi*(real(i)-1d0/2d0)/h;
        dzeta(i) = sqrt(kappa*kappa-an*an);
        print *, 'dzeta ', dzeta(i);
    enddo
        
    !t - полюса на вещественной оси. 0, если их нет
    t3=0d0;
    t4=0d0;
    tm=1d-2;
    tp=0d0;
    pr=1d-2;
    gr=5d3;
    eps=1d-4;
    
    !вычисление интеграла с помощью DINN5. Результаты записываются в Rd.
    call DINN5(UI,t1,t2,t3,t4,tm,tp,eps,pr,gr,Nx,Rd)
    
    !вычисление интеграла с помощью вычетов. Результаты записываются в u_res.
    do i = 1, Nx
        u_res(i)=0d0;
        do j = 1, Np
            u_res(i) = u_res(i) + (exp(-dzeta(j)*x(i)*cmplx(0,1)) * (exp(sigma(dzeta(j))*zj) - exp(-sigma(dzeta(j))*(zj+2d0*h)))) / (sigmapr(dzeta(j)) * (1d0+ exp(-2d0*sigma(dzeta(j))*h))); 
            print *, 'RES ', u_res(i);
            print *, 'CHISL1 EXP ', exp(-dzeta(j)*x(i)*cmplx(0,1));
            print *, 'CHISL2 ', (exp(sigma(dzeta(j))*zj) - exp(-sigma(dzeta(j))*(zj+2d0*h)));
            print *, 'ZNAM ', sigmapr(dzeta(j)) * (1d0 + exp(-2d0*sigma(dzeta(j))*h));
            print *, 'ZNAM2 ', (1d0 + exp(-2d0*sigma(dzeta(j))*h));
            print *, 'sigma ', sigma(dzeta(j));
            
            !u_res(i) = u_res(i) + (exp(dzeta(j)*x(i)*cmplx(0,1)) * (exp(sigma(-dzeta(j))*zj) - exp(-sigma(-dzeta(j))*(zj+2*h)))) / (sigmapr(-dzeta(j)) * (1+ exp(-2*sigma(-dzeta(j))*h)));
            !u_res(i) = u_res(i) + (exp(dzeta(j)*x(i)*cmplx(0,1)) * sinh(real(sigma(-dzeta(j))*(zj+h))) / (sigmapr(-dzeta(j)) * cosh(real(sigma(-dzeta(j))*h))));
        enddo
        u_res(i) = u_res(i) * cmplx(0,-1);
    enddo
    
    !запись данных в файлы
    open(unit = 1, file = 'UI/x.txt', status = 'unknown')
    open(unit = 2, file = 'UI/u.txt', status = 'unknown')
    open(unit = 3, file = 'UI/u_res.txt', status = 'unknown')    
    
    print *, 'x, |Rd|, |u_res|: '
    do i = 1, Nx
        !вывод в консоль
        print *, x(i), abs(Rd(i)), abs(u_res(i));
        !print *, x(i), real(Rd(i)), imag(Rd(i)), abs(Rd(i))
        print *
        !вывод в файлы
        Write(1,1) x(i)
        Write(2,1) abs(Rd(i))
        Write(3,1) abs(u_res(i))      
    enddo
    
    !!______________зависимость |u| от omega при x=const ___________________________________________________________________
    !xi=1;
    !Nom=60;
    !allocate(omega(Nom));
    !allocate(Uom(Nom));
    !do i = 1, Nom
    !    omega(i) = real(i)/10;
    !    print *, 'omega ', omega(i);
    !enddo
    !
    !!вычисление интеграла
    !call DINN5(UIomega,t1,t2,t3,t4,tm,tp,eps,pr,gr,Nom,Uom)
    !
    !!запись данных в файлы
    !open(unit = 4, file = 'UIomega/omega.txt', status = 'unknown')
    !open(unit = 5, file = 'UIomega/u.txt', status = 'unknown')    
    !
    !print *, 'omega, |Uom|: '
    !do i = 1, Nom
    !    !вывод в консоль
    !    print *, omega(i), abs(Uom(i));
    !    !print *, x(i), real(Rd(i)), imag(Rd(i)), abs(Rd(i))
    !    print *
    !    !вывод в файлы
    !    Write(4,1) omega(i)
    !    Write(5,1) abs(Uom(i))     
    !enddo
    
    deallocate(Rd, x, u_res, dzeta) !, omega, Uom
1   format(f10.5)
2   format(f10.5,2x,f20.4,2x,f20.15)     
    pause
    
    
    contains
    
    complex*16 function sigma(al)
        real*8 :: al, tempp
        
        if(kappa>abs(al)) then
            tempp=-sqrt(kappa*kappa-al*al);
            sigma=cmplx(0,tempp);
        else
            tempp=sqrt(al*al-kappa*kappa);
            sigma=cmplx(tempp,0);
        endif
    end function
    
    complex*16 function sigmapr(al)
        real*8 :: al, tempp
        
        if(kappa>abs(al)) then
            tempp=al/sqrt(kappa*kappa-al*al);
            sigmapr=cmplx(0,tempp);
        else
            tempp=al/sqrt(al*al-kappa*kappa);
            sigmapr=cmplx(tempp,0);
        endif
    end function
    
    end program Waveguide

