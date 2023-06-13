 
    subroutine UI(al,s,N)

    use global
    implicit none

    integer :: i, N    
    complex*16 :: al, s(N), sig
 
    sig=sqrt(al*al - ((kappa+cmplx(0,1.0e-12))*(kappa+cmplx(0,1.0e-12))));
    do i = 1, N
        s(i) = cos(al*x(i)) / pi * (exp(sig*z0)-exp(-sig*(z0+2*h))) / (sig*(1+exp(-2*sig*h))); 
    enddo

    !al - ����� - ��������� ��������������. �� ���� ��� DINN.
    !s  - ������, � ������� ����� �������� ����������� ��������������� ������� (��� ��������� �����).
    !N  - ���������� �����.
    end