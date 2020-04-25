C     **************************************************
C                  FLASH BOILING MODEL
C           n-tridecane & CO2 mixed Fuel version
C     **************************************************
C     MAIN PROGRAM
       DOUBLE PRECISION ML,MS,MV1,T,VG2,VL,VSP
       OPEN(10,FILE='../DATA/MV3.csv')     !AFRBKにおけるガス質量
        OPEN(18,FILE='../DATA/Bubble_Radius_before_Injection.csv') !噴射前気泡径R 20190522

        OPEN(28,FILE='../DATA/MV082.9.csv') !蒸気質量 300ステップ毎 sub.Temp内
        OPEN(29,FILE='../DATA/MOL082.9.csv') !噴霧mol量 300ステップ毎
        OPEN(30,FILE='../DATA/VG1.csv')      !BFRBKにおけるガス体積の差VG2-VG1
        OPEN(31,FILE='../DATA/DMV1-DMV2-DMV3.csv') !全蒸気質量変化量 sub.TEMP内
c       DMV1:蒸気質量変化量(気泡内)
c       DMV2:蒸気質量変化量(膜面)
c       DMV3:蒸気質量変化量(液滴表面)
        OPEN(32,FILE='../DATA/T_inside_nozzle.csv')   !液体温度(ノズル内部)
        OPEN(33,FILE='../DATA/T_outside_nozzle.csv')   !液体温度(ノズル外部)
        OPEN(101,FILE='../DATA/RDR_RSP.csv')   !RDR,RSP
        CALL ENTER(F,PB,PB1)
        CALL CAL(ML,MS,MV1,PB1,T,VG2,VL,VSP,Y,Y0,Z,RDR)
c        write(555,*)VG2,VSP
c        write(*,*)'RDR=',RDR
        CALL SPRAY(F,ML,MS,MV1,PB1,T,VG2,VL,VSP,Y,Y0,Z,RDR)
        STOP
        END

C     **********************************************
C　　　　　　　　　SUBROUTINE ENTER
C     **********************************************

        SUBROUTINE ENTER(F,PB,PB1)
c	WRITE(*,*)'背圧Pb(MPa)を入力'
c	READ(*,*)PB
       WRITE(*,*)'Calucurate time? [microsec]'
        READ(*,*)F
c      WRITE(*,*)'Pb(MPa)=',PB
        WRITE(*,*)'F(micro sec)=',F
	    F=F*1E-6
c            !背圧を[Pa]に変換
        PB1=35.E03 ! 14.E03 21.E03 35.E03 48.E03
        RETURN
        END

C     **********************************************
C          SUBROUTINE CAL  (管内部の計算)
C       (飽和蒸気圧以上の時の気泡の成長の計算)
C     **********************************************

      SUBROUTINE CAL(ML,MS,MV1,PB1,T,VG2,VL,VSP,Y,Y0,Z,RDR)
        REAL PRE(0:11000000)
        REAL YA(0:11000000)
        REAL K,M0,LAT,P0,PAI,PB1,PBB,Y,Y0,YP,PV,MUL
        DOUBLE PRECISION BUB,ML,MS,MV1,T,TSAT0,VG0,VG1,VG2,VL0,VL,VSP
        PAI=3.141592
c        common /RDR/RDR
C 雰囲気圧力は
        WRITE(*,*)'PB1=',PB1
C 演算回数Xを求める(Xは無次元数)
c        H=80.0E-9              !タイムステップは1.0*10-7(s)=0.1(μs)
       H=1.0E-10
c	X=0.9E-3/21.0/H       !INJECTORの管路長/噴射速度/タイムステップ
        X=0.527E-3/20.0/H       !INJECTORの管路長/噴射速度/タイムステップ
         INJ=NINT(X)
        WRITE(*,*)'INJ=',INJ      !計算回数を表示
C 差圧⊿P [MPa]:実験条件により変化させる
      DP1=250E03            !Pa=XXMPa]の場合
	  WRITE(*,*)'DP1=',DP1
C ベルヌーイの式より，インジェクターの内部の圧力を求める．
      P0=(PB1+DP1-125000.)*0.7    !損失係数は0.7　P0=Pinj-(ρ*v^2)/2
C      PRE(0)=P0     !管内圧 [MPa]
      WRITE(*,*)'Inside nozzle pressureP0=',P0

c 液滴径を0.5 mmとして設定．RDRの単位は[m]
      RDR=0.5E-3

c 初期気泡径は20μm
c 初期気泡半径は10μm
       YA(0)=20E-6/2.0
       Y0=20E-6/2.0      !初期気泡半径は10μm
       Y=20E-6/2.0
C 燃料の温度[K]
        T=293.
C 体積の計算
C NOZZLE OUTLET の初期ガス,液体体積
        write(*,*)'cal (phase2)'
c        stop
c        CALL PHASE2(PV,LAT,ST,CPL,DENL,DENG,RAM,TSAT0)
      CALL PHASE2(PV,LAT,ST,CPL,DENL,DENG,RAM,TSAT0,MUL,T,PB1)
c 計算で使用していない！！
         VOLIN=PAI*((931E-6/2.)**2-(798E-6/2.)**2)*0.527E-3 !pi* r^2 * INJECTORの管路長
C          BUB=1.11E12*EXP(-5.28/(T-TSAT0))*(10**(-4.34))/40000
        BUB=1.11E12*EXP(-5.28/(T-TSAT0))*(4/3*PAI*RDR**3)*1.
                    !↑噴射開始前の発生気泡数：のち時間と共に変化
       WRITE(20,*)'BUB_80=',BUB,EXP(-5.28/(T-TSAT0)),4/3*PAI*RDR**3
c       WRITE(*,*)'BUB_80=',BUB,EXP(-5.28/(T-TSAT0)),13.E-9
c       stop
c        VL0=PAI*((931E-6/2.)**2-(798E-6/2.)**2)*0.527E-3 !pi* r^2 * INJECTORの管路長    !初期の液相の体積 [m**3]
         VL0=4/3.*PAI*RDR**3
c G0---> VG0に変更（ミス？20190613）
         VG0=4/3.*PAI*(Y0**3)*BUB         !初期の気相の体積 [m**3]
      M0=VL0*DENL                      !初期の液相の質量 [kg]

c       WRITE(*,*)'l95 M0,VL0,DENL,VG0,BUB',M0,VL0,DENL,VG0,BUB
c       stop
C NOZZLE OUTLET の質量[kg]
      MV1=0.0
      ML=M0
      MS=M0
C 気泡径の変化で蒸発量が求まる
      VG2=VG0
	VG1=VG2
      PBB=PB1
C オリフィス内の計算
c       WRITE(*,*)'l106 VG0',VG0
c       WRITE(*,*)'l106 VG1',VG1
c       WRITE(*,*)'l106 VG2',VG2
c       stop
      PRE(0)=P0
      DO 1 I=1,INJ

	PRE(I)=P0-(P0-PBB)*I/NINT(X)

	PB1=PRE(I)

	IF(I.EQ.INJ) THEN
	ITBC=INJ
       Y=Y*(P0/PB1)**(1.0/3.0)
c	write(18,*)'goto11',I,Y
	GOTO 11
	ENDIF

	IF (PB1.GT.PV) THEN
	GOTO 1
	ELSE
            ITBC=I
                 GOTO 10
                        END IF

  1   CONTINUE

 10	Y=Y*(P0/PV)**(1.0/3.0)
c      write(18,*)'goto10',I,Y
 11	Y0=Y
       YA(1)=Y
C     ***********************************************
C             飽和蒸気圧以下の時の計算
C     ***********************************************　
C 時間による気泡変化率は，初期状態では0.0
	Z=0.0
       YP=Y0
C	WRITE(27,*)ITBC
	DO 2 I=ITBC+1,INJ
	PB2=PRE(I-1)
       PRE(I)=P0-(P0-PBB)*I/NINT(X)
	PB1=PRE(I)
       K=8.0E-6                         !表面粘性係数κC
        PGS=P0+2.*ST/10E-6
	YP=YP*((PB2/PB1)**(1.0/3.0))
	Y=YP
c	WRITE(18,*)'YP=',I,H*FLOAT(I),YP
	WRITE(18,*)H*FLOAT(I),YP

        CALL RUNGE1(H,PB1,PV,Y,Y0,Z,ST,DENL,K,MUL,PGS)
       YA(I)=Y
C 気泡の成長によって、蒸発量を求める
	TIME=H*FLOAT(I)                   !気泡数に時間の概念を入れる

        VG1=VG2
c        BUB=1.11E12*EXP(-5.28/(T-TSAT0))*(10**(-4.34*EXP(-5*TIME)))/40000
        BUB=1.11E12*EXP(-5.28/(T-TSAT0))*(4/3*PAI*RDR**3)*1.
	WRITE(20,*)'BUB_150=',BUB
c       WRITE(*,*)'BUB_150=',BUB
c       WRITE(*,*)'l163 VG0',VG0
c       WRITE(*,*)'l163 VG1',VG1
c       WRITE(*,*)'l163 VG2',VG2
C	BUB=1.11E12*EXP(-5.28/(T-TSAT0))*(10**(-4.34))/40000
        VG2=4/3.*PAI*(Y**3)*BUB
       write(21,*)'sub',VG2,Y,PB2,PB1     !20190606追加
c DMV1:蒸気質量変化量(気泡内)
	DMV1=(VG2-VG1)*DENG
	IF(T.EQ.TSAT0) DMV1=0.0
C 蒸発量によって，液体の量，気体になった量が求まる
	MV1=MV1+DMV1                !気相の増加
	ML=M0-MV1                   !液相の減少
	IF(ML.LE.0.0) ML=0.0
	T=T-DMV1*LAT/(CPL*ML)
c      WRITE(32,*)TIME,T,'Inside Injector', DMV1,CPL,ML,M0,MV1,DMV1
      WRITE(32,*)TIME,T

C	IF(T.GT.383.)THEN
C	T=383
C	ELSE
C	T=TSAT0
C	ENDIF
C	YP=Y
      Y1=Y+(YP-Y0)
      write(889,*)TIME,Y1
  2   CONTINUE
C  噴出直前の半径
C      Y=YA(INJ)
      Y=Y+(YP-Y0)
      Y0=Y
	VL=ML/DENL
	VSP=VL+VG2
	MS=ML+MV1
      WRITE(*,*)'2R=',Y*2
       WRITE(*,*)'  '
      WRITE(*,*)TIME,T,'Inside Injector', DMV1,CPL,ML,M0,MV1,DMV1
c       WRITE(*,*)'l190 VG0',VG0
c       WRITE(*,*)'l190 VG1',VG1
c       WRITE(*,*)'l190 VG2',VG2
c       WRITE(*,*)'l190 DMV1',DMV1
c       WRITE(*,*)'l190 MV1',MV1
c       WRITE(*,*)'l190 ML',ML
c       WRITE(*,*)'l190 M0',M0
c       stop


      RETURN
	END

C ************************************************************
C                     SUBROUTINE SPRAY
C ************************************************************

      SUBROUTINE SPRAY(F,ML,MS,MV1,PB1,T,VG2,VL,VSP,Y,Y0,Z,RDR)
C --------------------/ 配列の定義 /--------------------

	REAL K,RAM,LAT,MAIR,P0,PB1,PGS,PAI,Y0,MUL
	DOUBLE PRECISION ASP,BUB,DMV1,DMV2,DMV3,GRAD,M0,MOL,MS,ML
     &,MV,MV1,MV2,MV3,MVS,PU,RSP,RSPN,TA,VG2,VL,VSP,PU1
	DOUBLE PRECISION T,TSAT0
      INTEGER BK1
c        common /RDR/RDR

C ------------------------------------------------------
c       WRITE(*,*)'-------------------'
c       WRITE(*,*)'l224 F',F
c       WRITE(*,*)'l224 ML',ML
c       WRITE(*,*)'l224 MS',MS
c       WRITE(*,*)'l224 MV1',MV1
c       WRITE(*,*)'l224 PB1',PB1
c       WRITE(*,*)'l224 T',T
c       WRITE(*,*)'l224 VG2',VG2
c       WRITE(*,*)'l224 VL',VL
c       WRITE(*,*)'l224 VSP',VSP
c       WRITE(*,*)'l224 Y',Y
c       WRITE(*,*)'l224 Y0',Y0
c       WRITE(*,*)'l224 Z',Z
c       WRITE(*,*)'l224 RDR',RDR

c       stop
	PAI=3.141592
C 演算回数Qを計算する
c      H=80.0E-9
      H=1.0E-10
	Q=F/H      !噴射時間/タイムステップ＝演算回数
C 噴霧速度 VEKI(m/s) を設定 (赤外散乱からの計測速度)
C 噴射量は，いつも時間によらず一定
      VEKI=0.0   !VEKI=20.0噴霧の速度は今液滴一個で静止しているものとする．
C 気泡から液滴に変化，その際，蒸発

	TA=293.
      BK1=201  !使用していない!用途不明
C NOZZLE OUTLET の噴出直後のガス,液体体積&質量
	MV2=0.0
	MV3=0.0
	M0=ML
C	ML=M0   ! 意味があるのか？
	MV=0.0
C RUNGE-KUTTAの式を使うに当たって，初期条件を定める．
      K=8.0E-6  !表面粘性係数 subroutine calと同じ処理
      DP1=250E03 !差圧 subroutine calと同じ
      P0=(PB1+DP1-125000.)*0.7 !subroutine calと同じ
C 噴霧後の液滴の初期値,噴出直後の噴霧径
C Injector噴出後も，気泡の成長が連続していると考える
       RSP=0.0         !噴霧はないので0.0　　　元は0.931E-3 !噴霧外径
c       RDR=0.0      ! 液滴の半径 初期値0は必要か？　[m]
c       RDR=1.0E-3      ! 液滴の半径 設定が必要

c      CALL PHASE2(PV,LAT,ST,CPL,DENL,DENG,RAM,TSAT0)
       CALL PHASE2(PV,LAT,ST,CPL,DENL,DENG,RAM,TSAT0,MUL,T,PB1)

       PGS=P0+2.*ST/10E-6 !千田(2004)Pg0に相当
C 初期の噴霧表面積（液膜分裂前）を求める.
c    液滴一個なので表面積を求める必要なし？ VSP求めるのでイキ
      CALL SPAREA(ASP,DENL,GRAD,H,RSP,RSPN,ML,Q,VEKI,VG2,VL,VSP)

c       WRITE(*,*)'-------------------'
c       WRITE(*,*)'l224 ASP',ASP
c       WRITE(*,*)'l224 DENL',DENL
c       WRITE(*,*)'l224 GRAD',GRAD
c       WRITE(*,*)'l224 H,',H
c       WRITE(*,*)'l224 RSP',RSP
c       WRITE(*,*)'l224 RSPN',RSPN
c       WRITE(*,*)'l224 ML',ML
c       WRITE(*,*)'l224 Q',Q
c       WRITE(*,*)'l224 VEKI',VEKI
c       WRITE(*,*)'l224 VG2',VG2
c       WRITE(*,*)'l224 VL',VL
c       WRITE(*,*)'l224 VSP',VSP
c       WRITE(*,*)'l224 RDR',RDR
c       stop
C 噴霧体積:蒸気化のための熱量を受け取る周囲気体体積を仮定

c   噴霧内径外径の比は常に一定の6:7今のままでは円柱で仮定している
c　　ホロコーン噴霧を仮定
c   1-6^2/7^2　周囲気体は噴霧の体積の3倍を設定（根拠はなし）
c       MAIR=13/49.*PAI*(RSP**2)*H*VEKI !周囲気体体積
       MAIR=4/3*PAI*RDR**3*3
       WRITE(*,*)'l288 MAIR',MAIR
C      nflag=0

c
c
c ---------------------------------------------------------------
c
c
c
c  I, Jを区別してループを回す必要性を感じない。後述
c
       DO 500 J=1,201
       DO 6 I=(J-1)*NINT(Q)/200+1,J*NINT(Q)/200
C 泡の数は，時間とともに変化する
      TIME=FLOAT(I)*H

C ボイド率で液膜の分裂の判定を行なう
         PU=VG2/VSP
c        write(*,*)vg2,vsp
        WRITE(*,*)"PU,TIME=",PU,TIME,FLOAT(I)
c       stop
C	WRITE(21,*)'PU=',PU
	WRITE(23,*)VG2,VSP     !20190606追加
	PB=PB1/1000.
	PUC=0.55 !臨界ボイド率

C 臨界ボイド率による分裂判定
        IF (PU.LT.PUC) THEN

c       WRITE(*,*)'----------'
c       WRITE(*,*)'BFRBK'
C------------------------------------------------------------------
C                            分裂前
C                            IF (PU.LT.PUC) THEN
C------------------------------------------------------------------
C 分裂前の α1(kW/m2*K)を求める
c RAM w/(mK)
c RSP(m)
c ヌセルト数を2.0
c 20190710
cc        ALPHA=2.*RAM/(RSP/7.)*1000
c        ALPHA=2.*RAM/(RSP/7.)/1000
c液滴だけなので代表寸法Lには液滴の径を用いた
        ALPHA=2.*RAM/(RDR*2)/1000

C 気泡は，RAYLEIGH-PRESSETの式に基づいて，成長する
c	  IF(T.NE.TSAT0)THEN
c	  IFをなくす
c       WRITE(*,*)'Before Runge-------------------'
c       WRITE(*,*)'l330 H',H
c       WRITE(*,*)'l330 PB1',PB1
c       WRITE(*,*)'l330 PV',PV
c       WRITE(*,*)'l330 Y',Y
c       WRITE(*,*)'l330 Y0',Y0
c       WRITE(*,*)'l330 Z',Z
c       WRITE(*,*)'l330 ST',ST
c       WRITE(*,*)'l330 DENL',DENL
c       WRITE(*,*)'l330 K',K
c       WRITE(*,*)'l330 MUL',MUL
c       WRITE(*,*)'l330 PGS',PGS
c       WRITE(*,*)'ALPHA',ALPHA
	  CALL RUNGE1(H,PB1,PV,Y,Y0,Z,ST,DENL,K,MUL,PGS)
c       WRITE(*,*)'After Runge-------------------'
c       WRITE(*,*)'l330 H',H
c       WRITE(*,*)'l330 PB1',PB1
c       WRITE(*,*)'l330 PV',PV
c       WRITE(*,*)'l330 Y',Y
c       WRITE(*,*)'l330 Y0',Y0
c       WRITE(*,*)'l330 Z',Z
c       WRITE(*,*)'l330 ST',ST
c       WRITE(*,*)'l330 DENL',DENL
c       WRITE(*,*)'l330 K',K
c       WRITE(*,*)'l330 MUL',MUL
c       WRITE(*,*)'l330 PGS',PGS
c       WRITE(*,*)'After Runge-------------------'
c       stop
C	  WRITE(*,*)TIME,Y
c        END IF

C 分裂前のサブルーチン．必要な物性値を求めてから
C	CALL PHASE2(PV,LAT,ST,CPL,DENL,DENG,RAM,TSAT0)
c 計算で使用していない！！
       VOLIN=PAI*((931E-6/2.)**2)*0.527E-3 !pi* r^2 * INJECTORの管路長
c time依存しない 20190605
c       BUB=1.11E12*EXP(-5.28/(T-TSAT0))*(10**(-4.34*EXP(-5*TIME)))
c     &     /40000
           BUB=1.11E12*EXP(-5.28/(T-TSAT0))*(4/3*PAI*RDR**3)*1.
C	BUB=1.11E12*EXP(-5.28/(T-TSAT0))*(10**(-4.34))/40000
         WRITE(20,*)'BUB_281=',BUB
C         WRITE(*,*)'BUB_281=',BUB
C	WRITE(23,*)T,BUB
c    !!!!!!!!!!!!!!! miss!!! 20190613
	CALL BFRBK(ALPHA,ASP,BUB,DENG,H,J,DMV1,DMV2,DMV3,LAT,MV1,MV2
     & ,T,TA,TSAT0,VG2,Y,DMV4)
         write(885,*)TIME,Y
c         write(*,*)TIME,Y
c       WRITE(*,*)'-------------------'
c       WRITE(*,*)'l360 ALPHA',ALPHA
c       WRITE(*,*)'l360 ASP',ASP
c       WRITE(*,*)'l360 BUB',BUB
c       WRITE(*,*)'l360 DENG',DENG
c       WRITE(*,*)'l360 H',H
c       WRITE(*,*)'l360 J',J
c       WRITE(*,*)'l360 DMV1',DMV1
c       WRITE(*,*)'l360 DMV2',DMV2
c       WRITE(*,*)'l360 DMV3',DMV3
c       WRITE(*,*)'l360 LAT',LAT
c       WRITE(*,*)'l360 MV1',Mv1
c       WRITE(*,*)'l360 MV2',Mv2
c       WRITE(*,*)'l360 T',T
c       WRITE(*,*)'l360 TA',TA
c       WRITE(*,*)'l360 TSAT0',TSAT0
c       WRITE(*,*)'l360 VG2',VG2
c       WRITE(*,*)'l360 Y',Y
C 潜熱によって,気泡及び周囲の空気が温度が降下していく
c       WRITE(*,*)'start TEMP----------------'
c       WRITE(*,*)'l360 CPL',CPL
c       WRITE(*,*)'l360 DMV1',DMV1
c       WRITE(*,*)'l360 DMV2',DMV2
c       WRITE(*,*)'l360 DMV3',DMV3
c       WRITE(*,*)'l360 LAT',LAT
c       WRITE(*,*)'l360 MOL',MOL
c       WRITE(*,*)'l360 ML',ML
c       WRITE(*,*)'l360 MS',MS
c       WRITE(*,*)'l360 MV',MV
c       WRITE(*,*)'l360 MVS',MVS
c       WRITE(*,*)'l360 T',T
c       WRITE(*,*)'l360 TA',TA
c       WRITE(*,*)'l360 TSAT0',TSAT0
c       WRITE(*,*)'l360 TIME',TIME
c       WRITE(*,*)'l360 DMV4',DMV4
c       WRITE(*,*)'l360 I',I
        CALL TEMP(CPL,DMV1,DMV2,DMV3,LAT,MOL,ML,MS,MV,MVS
     &  ,T,TA,TSAT0,TIME,DMV4,I)
       WRITE(*,*)'end TEMP-------------------'
c       WRITE(*,*)'l360 CPL',CPL
c       WRITE(*,*)'l360 DMV1',DMV1
c       WRITE(*,*)'l360 DMV2',DMV2
c       WRITE(*,*)'l360 DMV3',DMV3
c       WRITE(*,*)'l360 LAT',LAT
c       WRITE(*,*)'l360 MOL',MOL
c       WRITE(*,*)'l360 ML',ML
c       WRITE(*,*)'l360 MS',MS
c       WRITE(*,*)'l360 MV',MV
c       WRITE(*,*)'l360 MVS',MVS
c       WRITE(*,*)'l360 T',T
c       WRITE(*,*)'l360 TA',TA
c       WRITE(*,*)'l360 TSAT0',TSAT0
c       WRITE(*,*)'l360 TIME',TIME
c       WRITE(*,*)'l360 DMV4',DMV4
c       WRITE(*,*)'l360 I',I
c       WRITE(*,*)'MAIR=',MAIR

c      stop
        CALL TEMPTA(DMV2,DMV3,LAT,MAIR,TA,T,TSAT0)

C　温度の低下により，噴霧の表面積が変わる
	CALL SPAREA(ASP,DENL,GRAD,H,RSP,RSPN,ML,Q,VEKI,VG2,VL,VSP)

	ELSE
C------------------------------------------------------------------
C                            分裂後-
C------------------------------------------------------------------
      H=1.0E-10
C 分裂して生じた液滴半径を求めるため、物性値を計算する

C TBKとは，分裂してからの時間である
c
c BK,BK1,TBKは使用していないが。。。。
c すなわちI, Jの区別は不要では？
c
      IF (TBK.LE.0) THEN
      BK=I
      BK1=J
      TBK=F*BK/(201*Q/200)
	END IF
c       CALL PHASE2(PV,LAT,ST,CPL,DENL,DENG,RAM,TSAT0)
      CALL PHASE2(PV,LAT,ST,CPL,DENL,DENG,RAM,TSAT0,MUL,T,PB1)

c time依存しない 20190605
c      BUB=1.11E12*EXP(-5.28/(T-TSAT0))*10**(-4.34*EXP(-5*TIME))/40000
           BUB=1.11E12*EXP(-5.28/(T-TSAT0))*(4/3*PAI*RDR**3)*1.
     	WRITE(20,*)'BUB_315=',BUB,T
C       write(1001,*)'before AFTBK',T,TSAT0,PAI,RDR
      CALL AFTBK(ALPHA,BUB,DENL,DMV1,DMV2,DMV3,GRAD,H,LAT,ML,MV3
     &,Q,RDR,RSP,T,TA,Y,ALPHA2,DMV4,PB1)
C       write(1001,*)'after AFTBK',T,TSAT0,PAI,RDR

c  追記 20190522

      if(MV3 .GT. M0)THEN
         write(500,*)'Droplet is evaporated at (s)',TIME MV3
         stop
      else
	       WRITE(10,*)TIME,MV3
      endif

C α1 (KW/m2K) (分裂後)
C 熱伝導率が変化するので，αを改めて計算する必要がある．

C 代表長さは2*RDRとする．
C ヌッセルト数は2.0とする．

	ALPHA=(2*RAM/(2*RDR))/1000.
      IF(T-TSAT0.LE.5) THEN
    	ALPHA2=.76*(T-TSAT0)**.26

    	ELSEIF(T-TSAT0.LE.25) THEN
    	ALPHA2=.027*(T-TSAT0)**2.33
    	ELSE
c 変更20190710
c    	ALPHA2=8*LOG((T-TSAT0)-24)+46.8
    	ALPHA2=1.e+04*(T-TSAT0)**(-1.66)
    	ENDIF
       WRITE(*,*)'ALPHA,ALPHA2,T,TSAT0',ALPHA,ALPHA2,T,TSAT0
C 液滴の発生のため生じた潜熱で、雰囲気温度が変化する
c	MAIR=13./49.*PAI*(RSP**2)*H*VEKI
       MAIR=4/3.*PAI*RDR**3*3
	CALL TEMP(CPL,DMV1,DMV2,DMV3,LAT,MOL
     &,ML,MS,MV,MVS,T,TA,TSAT0,TIME,DMV4,I)
	CALL TEMPTA(DMV2,DMV3,LAT,MAIR,TA,T,TSAT0)
c      write(102,*)'AFTBK',time,RDR,RSP,PU
C	WRITE(*,*)T
C      PU1=PU
C      nflag=nflag+1
      END IF
c      write(101,*)time,RDR,RSP,PU,PU1
      write(101,*)time,RDR,RSP
C       write(*,*)nflag,PU,PU1
C      if ((PU .EQ. PU1) .and. (nflag .GT. 10)) stop

    6	CONTINUE
  500 CONTINUE
      RETURN
      END

C ---------------------------------------------------------------------
C                       噴霧表面積（液膜分裂前）
C ---------------------------------------------------------------------
      SUBROUTINE SPAREA(ASP,DENL,GRAD,H,RSP,RSPN,ML,Q,VEKI,VG2,VL,VSP)
	REAL PAI
	DOUBLE PRECISION ASP,GRAD,ML,RSP,RSP1,RSPN,VG2,VL,VSP
c          common /RDR/RDR
c DZ=VEKI*H !DZ=H x VEKI = TIME STEPあたりの噴霧長さ
	PAI=3.141592
C 外径の変化量を求めるために定義する
      RSP1=RSP
C 液体体積 VL
      VL=ML/DENL
C 噴霧体積 VSP:液体の体積+蒸気の体積
      VSP=VL+VG2
C 噴霧表面積 ASP
c     ASP=(52.*PAI*DZ*VSP)**0.5
      ASP=4*PAI*RSP**2
C 噴霧外径 RSP
c       RSP=(49.*VSP/13./PAI/DZ)**0.5*1000  !change
c       RSP=(3.*49.*VSP/13./PAI/DZ)**0.5  !change
      GRAD=(RSP-RSP1)/Q
c      write(1112,*)DZ,VEKI,H
c      write(1112,*)DENL,ML,VL
c      write(1112,*)RSP,ASP,VSP,VL,VG2
cc      stop
      RETURN
      END

C ----------------------------------------------------------------------
C                           分裂前の計算
C ----------------------------------------------------------------------
      SUBROUTINE BFRBK(ALPHA,ASP,BUB,DENG,H,J,DMV1,DMV2,DMV3,LAT,MV1,MV2
     &,T,TA,TSAT0,VG2,Y,DMV4)
	REAL LAT,MW,MWTRI,MWCO,PAI,YTRI,YCO
	DOUBLE PRECISION ASP,BUB,DMV1,DMV2,DMV3,MV1,MV2,T,TA,TSAT0,VG1,VG2
	INTEGER IBK,J
c          common /RDR/RDR

	PAI=3.14159
C 初期条件
c      MWTRI=184.35
c      MWCO=44.01
c      YTRI=(0.2*MWTRI)/(0.2*MWTRI+0.8*MWCO)
c      YCO=(0.8*MWCO)/(0.2*MWTRI+0.8*MWCO)
c      MW=MWTRI*YTRI+MWCO*YCO
        MW=72.151
C ガス体積(気泡内)
      VG1=VG2
      VG2=4./3.*PAI*(Y**3)*BUB
c	write(887,*)time,y
	WRITE(30,*)VG2-VG1
	WRITE(26,*)VG1,VG2     !20190606追加
C 蒸気質量変化量(気泡内)
C 過熱度が大きくなると，熱の供給が始まる．
C それまでは、熱の供給はない
      DMV1=(VG2-VG1)*DENG
C	IF(DMV1.LE.0.0) DMV1=0.0
C DMV2:蒸気質量変化量(膜面)

c !!!!!!!!!!miss 20190613
c      DMV2=ALPHA*(T-TA)/LAT*ASP*H
      DMV2=ALPHA*(TA-T)/LAT*ASP*H
      IF(DMV2.LE.0.0) DMV2=0.0

      DTS=T-TSAT0
      IF(DTS.LE.0.0) DMV1=0.0
C DMV3:蒸気質量変化量(液滴表面)
      DMV3=0.0
      IBK=J
C 液滴の半径は0
      DMV4=0.0
c      RDR=0.0
C 各々の質量を計算
      MV1=MV1+DMV1
      MV2=MV2+DMV2
	IF(MV1.LE.0.0) MV1=0.0
	IF(MV2.LE.0.0) MV2=0.0
      RETURN
      END

C --------------------------------------------------------------
C                         燃料温度の計算
C --------------------------------------------------------------
      SUBROUTINE TEMP(CPL,DMV1,DMV2,DMV3,LAT,MOL
     &,ML,MS,MV,MVS,T,TA,TSAT0,TIME,DMV4,I)
      REAL LAT
      REAL MW,MWCO,MWTRI
	DOUBLE PRECISION DMV,DMV1,DMV2,DMV3,ML,MOL,MS,MV,MVS,T,TA,TSAT0
c          common /RDR/RDR

c      MWTRI=184.35
c      MWCO=44.01
c      YTRI=(0.2*MWTRI)/(0.2*MWTRI+0.8*MWCO)
c      YCO=(0.8*MWCO)/(0.2*MWTRI+0.8*MWCO)
c      MW=MWTRI*YTRI+MWCO*YCO
      MVS=0.0 !add 20190613
      MW=72.151
C 全蒸気質量変化量
      DMV=DMV1+DMV2+DMV3+DMV4
      do 66 JJ=300,30000000,300
	if(I.eq.JJ)then
	WRITE(31,*)TIME,DMV1,DMV2,DMV3 !TIME追加
      endif
 66   continue
C 蒸気質量
      MV=MV+DMV
      do 67 JJ=300,30000000,300
	if(I.eq.JJ)then
	WRITE(28,*)TIME,MV
      endif
 67   continue
C 液体質量
      ML=ML-DMV
      IF (ML.LE.0.)THEN
        ML=0.0
        goto 50     !20190606追加
      ENDIF
C 噴霧質量
      MS=ML+DMV1
c      MVS=MVS+MV
      MVS=MV+ML  !20200205
      MOL=MVS/MW
      do 68 JJ=300,30000000,300
	if(I.eq.JJ)then
	WRITE(29,*)TIME,MOL
      endif
 68   continue
C 田中の修論によればTとTAST0の関係を設定するために以下を追加
      IF(T.LE.TSAT0)THEN     !20190606追加
         T=TSAT0 GOTO 50     !20190606追加
      ELSE     !20190606追加
C 温度降下の計算
C 成長による蒸発による温度降下
	T1=DMV1*LAT/(ML*CPL)
C	IF(T1.LE.0.0) T1=0.0
C	WRITE(25,*)DMV1,ML
C 潜熱による蒸発による温度降下
	T2=ABS((T-TSAT0)/(TA-TSAT0))*(DMV2+DMV3)*LAT/(ML*CPL)
C	IF(T2.LE.0.0) T2=0.0
C	WRITE(24,*)T1,T2
      T=T-T1-T2
      ENDIF     !20190606追加
      WRITE(15,*)'TEMP',T,T1,T2,DMV1,DMV2,DMV3,ML     !20190606追加
      do 69 JJ=300,30000000,300
	if(I.eq.JJ)then
c	    WRITE(32,*)TIME,T,T1,T2,DMV1,LAT,ML,CPL
	    WRITE(33,*)TIME,T

      endif
 69   continue
   50 RETURN
      END
C ------------------------------------------------------
C                   雰囲気温度の計算
C ------------------------------------------------------
      SUBROUTINE TEMPTA(DMV2,DMV3,LAT,MAIR,TA,T,TSAT0)
	REAL LAT,MAIR
	DOUBLE PRECISION DMV2,DMV3,T,TA,TSAT0
c          common /RDR/RDR

C 温度降下の計算

      CPA=1.006 !空気の比熱

C MAIRとは，噴霧周りの空気の量
      TA=TA-(TA-T)/(TA-TSAT0)*(DMV2+DMV3)*LAT/(CPA*MAIR)
	WRITE(15,*)'TEMPTA',TA,T     !20190606追加
      RETURN
      END

C     ****************************************************
C                SUBROUTINE PHASE2 (物性値の計算)
C     ****************************************************

c      SUBROUTINE PHASE2(PV,LAT,ST,CPL,DENL,DENG,RAM,TSAT0)
      SUBROUTINE PHASE2(PV,LAT,ST,CPL,DENL,DENG,RAM,TSAT0,MUL,T,PB1)
c−−−−−−−−−−PHASE2にTSAT0，PVを計算するためにT,PB1の引数を追加

      REAL PV,LAT,LOL,LOV,ST,CPL,DENL,DENG,RAM,MUL
      DOUBLE PRECISION TSAT0,T
c        common /RDR/RDR

c      PV=56.5E03          !飽和蒸気圧:[Pa]
      PV=24527*exp(0.0414*(T-273))

      LAT=385*exp(-19.7E-04*(T-273))!370.2                       !気化潜熱:[kJ/kg]

      LOL=626.               ![kg/m3]
      LOV=0.6324*exp(0.06719*(T-273))!2.424!0.6894                ![kg/m3] 20190925物性値変更(BASICに合わせる)

      ST=0.0182*exp(-6.447E-03*(T-273))!16.E-3                     !表面張力:[N/m]

c      TSAT0=261.                     !*Pa=14kPaでの飽和蒸気温度 [K]
      TSAT0=log(PB1/24527)/0.0414+273

      CPL=2.35                    !液相の比熱:[kJ/kg・K]

      DENL=LOL                        !液相の密度:[kg/m**3]
      DENG=LOV                        !気相の密度:[kg/m**3]

      RAM=0.1112                !熱伝導率

      MUL=2.831E-04*exp(-9.095E-03*(T-273))!240.E-6
      RETURN
	END

C *****************************************************************
C                      SUBROUTINE RUNGE1
C *****************************************************************

      SUBROUTINE RUNGE1(H,PB1,PV,Y,Y0,Z,ST,DENL,K,MUL,PGS)
      REAL H,K,KS,LS,MUL,PB1,PV,PGS,ST,Y0,YG,YI,ZG,ZI
     &    ,X(0:4),YZ(0:4),ZZ(0:4),OK(0:4),L(0:4),KD(0:4)
     &    ,R(0:4),RD(0:4),Q(0:4),QD(0:4),FNFGS,FNGGS
c             common /RDR/RDR

C     ---------------- FUNCTION -----------------------------------------
      FNFGS(YI,ZI)=ZI
      FNGGS(YI,ZI)=-(3/2.)*(ZI**2)/YI+(PV-PB1+(PGS*(Y0/YI)**3)
     &-2*ST/YI-4*MUL*ZI/YI-4*K*ZI/(YI**2))/(DENL*YI)
C-----------------------------------------------------------------------
C	WRITE(*,*)'RU=',Y
C	WRITE(*,*)'FNGGS=',FNGGS(YI,ZI)
	YI=Y
	ZI=Z
	Q(0)=0.0
      QD(0)=0.0
      YZ(0)=YI
      ZZ(0)=ZI
      YG=YZ(0)
      ZG=ZZ(0)
      DO 7 KK=0,3
        OK(KK)=H*FNFGS(YG,ZG)
        L(KK) =H*FNGGS(YG,ZG)
        IF (KK.EQ.0) THEN
          R(1)=(OK(KK)-2*Q(KK))/2.
          GOTO 100
        ELSE IF (KK.EQ.1) THEN
          R(2)=(OK(KK)-Q(KK))*(1-SQRT(1/2.))
          GOTO 100
        ELSE IF (KK.EQ.2) THEN
          R(3)=(OK(KK)-Q(KK))*(1+SQRT(1/2.))
          GOTO 100
        END IF
        R(4)=(OK(KK)-2*Q(KK))/6.
  100   YZ(KK+1)=YZ(KK)+R(KK+1)*H
        ZZ(KK+1)=ZZ(KK)+R(KK+1)*H
        R(KK+1)=(YZ(KK+1)-YZ(KK))/H
        IF (KK.EQ.0) THEN
          Q(1)=Q(KK)+3*R(KK+1)-OK(KK)/2.
          GOTO 101
        ELSE IF (KK.EQ.1) THEN
          Q(2)=Q(KK)+3*R(KK+1)-(1-SQRT(1/2.))*OK(KK)
          GOTO 101
        ELSE IF (KK.EQ.2) THEN
          Q(3)=Q(KK)+3*R(KK+1)-(1+SQRT(1/2.))*OK(KK)
          GOTO 101
        END IF
        Q(4)=Q(KK)+3*R(KK+1)-OK(KK)/2.
  101   KD(KK)=1.
        IF (KK.EQ.0) THEN
          RD(1)=(KD(KK)-2*QD(KK))/2.
          GOTO 102
        ELSE IF (KK.EQ.1) THEN
          RD(2)=(KD(KK)-QD(KK))*(1-SQRT(1/2.))
          GOTO 102
        ELSE IF (KK.EQ.2) THEN
        RD(3)=(KD(KK)-QD(KK))*(1+SQRT(1/2.))
        GOTO 102
        END IF
        RD(4)=(KD(KK)-2*QD(KK))/6.
  102   X(KK+1) =X(KK)+RD(KK+1)*H
        RD(KK+1)=(X(KK+1)-X(KK))/H
        IF (KK.EQ.0) THEN
          QD(1)=QD(KK)+3*RD(KK+1)-KD(KK)/2.
          GOTO 103
        ELSE IF (KK.EQ.1) THEN
          QD(2)=QD(KK)+3*RD(KK+1)-(1-SQRT(1/2.))*KD(KK)
          GOTO 103
        ELSE IF (KK.EQ.2) THEN
          QD(3)=QD(KK)+3*RD(KK+1)-(1+SQRT(1/2.))*KD(KK)
          GOTO 103
        END IF
        QD(4)=QD(KK)+3*RD(KK+1)-KD(KK)/2.
  103   XG=X(KK+1)
        YG=YZ(KK+1)
        ZG=ZZ(KK+1)
   7  CONTINUE
      Q(0)=Q(4)
      QD(0)=QD(4)
      KS=OK(0)/6.+(1-SQRT(1/2.))*OK(1)/3.+(1+SQRT(1/2.))*OK(2)/3.
     &+OK(3)/6.
      LS=L(0)/6.+(1-SQRT(1/2.))*L(1)/3.+(1+SQRT(1/2.))*L(2)/3.+L(3)/6.
      YI=YI+KS
      ZI=ZI+LS
C	WRITE(18,*)'Y=,',Y,'   Z=',Z
	Y=YI
	Z=ZI
	RETURN
      END
C ---------- 分裂後の計算 ----------
      SUBROUTINE AFTBK(ALPHA,BUB,DENL,DMV1,DMV2,DMV3,GRAD,H,LAT,ML,MV3
     &,Q,RDR,RSP,T,TA,Y,ALPHA2,DMV4,PB1)
c−−−−−−−−−−AFTBKにTSAT0を計算するためにPB1の引数を追加

	REAL LAT,MW,MWTRI,MWCO
	REAL PAI
	DOUBLE PRECISION BUB,DMV1,DMV2,DMV3,GRAD,MDR,ML,MV3,RSP
     &,T,TA,VDR
c             common /RDR/RDR

c      MWTRI=184.35
c      MWCO=44.01
c	YTRI=(0.2*MWTRI)/(0.2*MWTRI+0.8*MWCO)
c      YCO=(0.8*MWCO)/(0.2*MWTRI+0.8*MWCO)
c      MW=MWTRI*YTRI+MWCO*YCO
        MW=72.151

c      TSAT0=261.
c-------------20190717追加---
      TSAT0=log(PB1/24527)/0.0414+273
c----------------------------

	PAI=3.141592
C 分裂後は，正比例で成長していく
      RSP=GRAD*Q+RSP
      DMV1=0.0
      DMV2=0.0
C 液滴が生じるので、気泡の半径は0
      Y=0.0
C 20200227 The value of BUB is larger equal than 1
       if(BUB .LT. 1.0)stop

C MDR:液滴の質量 VDR:液滴の体積
      MDR=ML/2./BUB
      VDR=MDR/DENL
C RDR:液滴の半径 SADR:液滴の全表面積
      RDR=(0.75*VDR/PAI)**(1/3.)
      SADR=4.*PAI*2*(RDR**2)*BUB
      DMV3=ALPHA*ABS(TA-T)/LAT*SADR*H
      WRITE(1000,*)TA,T,DMV3,SADR,BUB,RDR,VDR,MDR,ML
c      stop
	DMV4=ALPHA2*(T-TSAT0)/LAT*SADR*H
	MV3=MV3+DMV3+DMV4

	IF(MV3.LE.0.0) MV3=0.0
c コメントする20190522
c	WRITE(10,*)MV3
      RETURN
      END
