    module global
    
    real*8, allocatable :: x(:), z(:), omega(:)
    
    real*8 :: c, f, h, kappa, x0,xn,z0,zn
    integer :: Nx,Nz
    
    
    real(8) pi;       parameter (pi=3.141592653589793d0)
    complex(8) cci;   parameter(cci=(0d0,1d0))
        
    contains
    
!   гиперболический синус комплексного числа
    complex*16 function sh(y)
        complex*16 :: y
        sh = sinh(real(y)) * cos(imag(y))  +  cosh(real(y)) * cci * sin(imag(y));        
    end function
        
!   гиперболический косинус комплексного числа
    complex*16 function ch(y)
        complex*16 :: y
        ch = cosh(real(y)) * cos(imag(y))  +  sinh(real(y)) * cci * sin(imag(y));        
    end function
    
    real*8 function an(n)
        integer :: n
        an = pi*(n-0.5d0)/h;        
    end function
    
    end module