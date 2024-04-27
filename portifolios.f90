        program agora

        DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE:: results,retorno
        DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE:: datas,atv4,MCor
        DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE:: atv1,atv2,atv3
        integer ano,mes,dia,operacoesAT1,operacoesAT2,totalDIAS
        double precision opr,capitali
        double precision rst1,rst2,rst3,rst4
        double precision retMed(2),desvio(2)
        double precision suma,sum1,EPoRe,sumvola,ISharpe,WVec(2),vola
        integer NA1,jafoi,esco,ctes,kbt,tbk
        double precision pc

        double precision grnd



        totalDIAS = 1610
        operacoesAT1 = 1491
        operacoesAT2 = 1459
        
        
        OPEN(51, FILE="WIN.txt", status="old")
        OPEN(52, FILE="WDO.txt", status="old")
        OPEN(55, FILE="diasBTC.txt", status="old")


        ALLOCATE(atv1(5,operacoesAT1),atv2(5,operacoesAT2))
        ALLOCATE(datas(3,totalDIAS),results(5,totalDIAS+1))
        ALLOCATE(retorno(5,totalDIAS),MCor(2,2))

        call sgrnd(1321554)!31167285


        open(11,file='dateF.dat')
        open(12,file='retornoD.dat')
        open(13,file='Portifolios.dat')
        open(10,file='resultados.dat')



!       GRAVANDO ATIVOS NA MEMORIA PARA MANIPULAR MAIS FACILMENTE
        do i = 1,totalDIAS
        read(55,*)ano,mes,dia
        datas(1,i) = ano
        datas(2,i) = mes
        datas(3,i) = dia
        end do

        do i = 1,operacoesAT1
        read(51,*)ano,mes,dia,opr,capitali
        atv1(1,i) = ano
        atv1(2,i) = mes
        atv1(3,i) = dia
        atv1(4,i) = opr
        atv1(5,i) = capital1
        end do
        

        do i = 1,operacoesAT2
        read(52,*)ano,mes,dia,opr,capitali
        atv2(1,i) = ano
        atv2(2,i) = mes
        atv2(3,i) = dia
        atv2(4,i) = opr
        atv2(5,i) = capital1
        end do


!       ORGANIZANDO PARA CADA DIA OPERAVEL
        results = 0.0

        do i = 1,totalDIAS
        ano = datas(1,i)
        mes = datas(2,i)
        dia = datas(3,i)

        results(1,i+1) = ano
        results(2,i+1) = mes
        results(3,i+1) = dia

        rst1 = 0.0
        rst2 = 0.0

        do j = 1,operacoesAT1
        if((atv1(1,j) .eq. ano) .and. (atv1(2,j) .eq. mes) &
            .and. (atv1(3,j) .eq. dia))then
            rst1 = rst1 + atv1(4,j)
        end if
        end do
        results(4,i+1) = results(4,i) + rst1

        do j = 1,operacoesAT2
        if((atv2(1,j) .eq. ano) .and. (atv2(2,j) .eq. mes) &
            .and. (atv2(3,j) .eq. dia))then
            rst2 = rst2 + atv2(4,j)
        end if
        end do
        results(5,i+1) = results(5,i) + rst2


        end do


!       ESCREVENDO OS RESULTADOS NOS ARQUIVOS
        do i = 1,(totalDIAS+1)

        write(10,*)results(1,i),results(2,i),results(3,i), &
                  results(4,i),results(5,i)

        end do



!       CALCULANDO OS RETORNOS
        do i = 2,totalDIAS

        retorno(1,i-1) = results(1,i)
        retorno(2,i-1) = results(2,i)
        retorno(3,i-1) = results(3,i)

        do j = 4,5
        if((results(j,i+1) .ne. 0.0) .and. (results(j,i) .ne. 0.0))then
        retorno(j,i-1) = (results(j,i+1)-results(j,i))/results(j,i)
        else
        retorno(j,i-1) = 0.0
        endif
        end do

        end do
        
        retMed(1) = sum(retorno(4,:))/dble(totalDIAS-1)
        retMed(2) = sum(retorno(5,:))/dble(totalDIAS-1)


!       ESCRERVENDO O RETORNO NO ARQUIVO
        do i = 1,(totalDIAS-2)
        write(12,*)retorno(1,i),retorno(2,i), &
                   retorno(3,i),retorno(4,i), &
                   retorno(5,i)
        enddo

!       DESVIO PADRAO
        do i = 4,5
        suma = 0.0
        do j = 1,(totalDIAS-1)
        suma = suma + (retorno(i,j) - retMed(i-3))**2
        end do
        desvio(i-3) = sqrt((suma/dble(totalDIAS-1)))
        end do





!       CALCULANDO matriz de covariancia = variancia correlacionada:      c_x_y = [SUM_i{(x_i-<x>)*(y_i-<y>)}]/N
        write(*,*)"============================"
        write(*,*)"|| Matriz de Covariancia  ||"
        write(*,*)"============================"
        write(*,*)"||"
        do l = 4,5
        do k = 4,5

        sum1 = 0.0
        do m = 1,(totalDIAS-1)
        sum1 = sum1 + ( (retorno(l,m)-retMed(l))* &
                        (retorno(k,m)-retMed(k))  )
        enddo
        MCor(l-3,k-3) = (sum1/dble(totalDIAS-1))

        enddo

        if(l .eq. 5)then
        write(*,*)"|| ",MCor(l-3,:)," ||"
        write(*,*)"||"
        write(*,*)"============================"
        else
        write(*,*)"|| ",MCor(l-3,:)," ||"
        endif

        enddo

        write(*,*)"============================"
        write(*,*)"||   Matriz de Codesvio   ||"
        write(*,*)"============================"
        write(*,*)"||"
        do l = 1,2
        write(*,*)"|| ",sqrt(sqrt(MCor(l,:)**2))," ||"
        enddo
        write(*,*)"||"
        write(*,*)"============================"





!       MC: Escolhendo aleatoriamente os portifolios
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       Porcentagens Iguais

        vola = 0.0
        WVec = 0.0
        ISharpe = 0.0
        sumvola = 0.0

        do i = 1,2
        WVec(i) = 1.0/2.0
        enddo
        EPoRe = 0.0
        do i = 1,2
        EPoRe = EPoRe + retMed(i)*WVec(i)
        enddo

        do i = 1,2
        do j = 1,2
        sumvola = sumvola + MCor(i,j)*WVec(i)*WVec(j)
        end do
        end do

        !volatilidade e retorno anual, multiplica a diaria por 252
        vola = sqrt(sumvola)
        EPoRe = EPoRe*252.0
        
        ISharpe = (EPoRe - 0.0)/vola
        write(13,*)1,WVec,sum(WVec),EPoRe,vola,ISharpe

!**************************************************************


!**************************************************************
!       Carteiras de 1 ativo
        do l = 1,2

        vola = 0.0
        WVec = 0.0
        ISharpe = 0.0
        sumvola = 0.0

        WVec(l) = 1.0

        EPoRe = 0.0
        do i = 1,2
        EPoRe = EPoRe + retMed(i)*WVec(i)
        enddo

        do i = 1,2
        do j = 1,2
        sumvola = sumvola + MCor(i,j)*WVec(i)*WVec(j)
        end do
        end do

        !volatilidade e retorno anual, multiplica a diaria por 252
        vola = sqrt(sumvola)
        EPoRe = EPoRe*252.0

        ISharpe = (EPoRe - 0.0)/vola
        write(13,*)l+1,WVec,sum(WVec),EPoRe,vola,ISharpe
        end do




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       Aleatorias
        NA1 = 1000
        do na = 1,NA1

!       CALCULANDO peso de cada ATIVO
!       Escolhendo ordem dos ativos aleatóriamente
        vola = 0.0
        WVec = 0.0
        ISharpe = 0.0
        sumvola = 0.0

        pc = 1.0
        esco = 2.0*grnd() + 1.0
        WVec(esco) = pc*grnd()
        pc = pc - WVec(esco)
        if(esco .eq. 1)then
        WVec(2) = pc
        else
        WVec(1) = pc
        endif


!       Retorno esperado da carteira
        EPoRe = 0.0
        do i = 1,2
        EPoRe = EPoRe + retMed(i)*WVec(i)
        enddo

        do i = 1,2
        do j = 1,2
        sumvola = sumvola + MCor(i,j)*WVec(i)*WVec(j)
        end do
        end do

        !volatilidade e retorno anual, multiplica a diaria por 252
        vola = sqrt(sumvola)
        EPoRe = EPoRe*252.0

        ISharpe = (EPoRe - 0.0)/vola
        write(13,*)na+l,WVec,sum(WVec),EPoRe,vola,ISharpe

        enddo







        write(*,*)retMed(:)
        write(*,*)desvio(:)
        read(*,*)



        end program






!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      MERSENNE TWISTER RANDOM NUMBER GENERATOR


      subroutine sgrnd(seed)

      implicit integer(a-z)

      parameter(N     =  624)

      dimension mt(0:N-1)

      common /block/mti,mt
      save   /block/


      mt(0)= iand(seed,-1)
      do 1000 mti=1,N-1
        mt(mti) = iand(69069 * mt(mti-1),-1)
 1000 continue

      return
      end

      double precision function grnd()

      implicit integer(a-z)

      parameter(N     =  624)
      parameter(N1    =  N+1)
      parameter(M     =  397)
      parameter(MATA  = -1727483681)
      parameter(UMASK = -2147483648.0)
      parameter(LMASK =  2147483647)
      parameter(TMASKB= -1658038656)
      parameter(TMASKC= -272236544)

      dimension mt(0:N-1)
      common /block/mti,mt
      save   /block/
      data   mti/N1/

      dimension mag01(0:1)
      data mag01/0, MATA/
      save mag01
      TSHFTU(y)=ishft(y,-11)
      TSHFTS(y)=ishft(y,7)
      TSHFTT(y)=ishft(y,15)
      TSHFTL(y)=ishft(y,-18)

      if(mti.ge.N) then
        if(mti.eq.N+1) then
          call sgrnd(4357)
        endif

        do 1000 kk=0,N-M-1
            y=ior(iand(mt(kk),UMASK),iand(mt(kk+1),LMASK))
            mt(kk)=ieor(ieor(mt(kk+M),ishft(y,-1)),mag01(iand(y,1)))
 1000   continue
        do 1100 kk=N-M,N-2
            y=ior(iand(mt(kk),UMASK),iand(mt(kk+1),LMASK))
            mt(kk)=ieor(ieor(mt(kk+(M-N)),ishft(y,-1)),mag01(iand(y,1)))
 1100   continue
        y=ior(iand(mt(N-1),UMASK),iand(mt(0),LMASK))
        mt(N-1)=ieor(ieor(mt(M-1),ishft(y,-1)),mag01(iand(y,1)))
        mti = 0
      endif

      y=mt(mti)
      mti=mti+1
      y=ieor(y,TSHFTU(y))
      y=ieor(y,iand(TSHFTS(y),TMASKB))
      y=ieor(y,iand(TSHFTT(y),TMASKC))
      y=ieor(y,TSHFTL(y))

      if(y.le.0) then
        grnd=(dble(y)+2.0d0**32)/(2.0d0**32-1.0d0)
      else
        grnd=dble(y)/(2.0d0**32-1.0d0)
      endif
      if(grnd .ge. 1.0)then
        grnd = 0.99
      end if

      return
      end
