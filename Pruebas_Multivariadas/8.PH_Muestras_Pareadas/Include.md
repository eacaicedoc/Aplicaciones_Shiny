#Prueba de hipótesis acerca de dos vectores de medias poblacionales $\boldsymbol{\mu_{1}}$ y $\boldsymbol{\mu_{2}}$, Observaciones Paradeas

Sea $\boldsymbol{X_1},\boldsymbol{X_2},...,\boldsymbol{X_n}$ y $\boldsymbol{Y_1},\boldsymbol{Y_2},...,\boldsymbol{Y_n}$ dos muestras aleatorias de una población p-variada con vectores de medias $\boldsymbol{\mu_{X}}$, $\boldsymbol{\mu_{Y}}$ desconocidos y matriz de varianzas-covarianzas $\Sigma$ desconocida
	
Suponga además que $Cov\left[X_i,Y_i\right]=\Sigma\neq 0$, $i=1,2,...,n$, es decir, las dos muestras aleatorias son correlacionadas (Muestras no independientes o dependientes)
	
Para contrastar las hipótesis:
	
$\begin{cases} H_{ 0 }:\quad \quad \boldsymbol{\mu_{X}}-\boldsymbol{\mu_{Y}} =0 \\ H_a: \quad \quad \boldsymbol{\mu_{X}}-\boldsymbol{\mu_{Y}} \neq 0  \end{cases}$
	
Se trabaja con las diferencias de cada para de observaciones multivariadas, definidas como:
	
$\boldsymbol{D_i}=\boldsymbol{X_i}-\boldsymbol{Y_i}$
	
Se asume que estas n-diferencias tienen una distribución normal multivariada con vector de medias $\boldsymbol{\mu}=0$ y matriz de varianzas-covarianzas dada por $\Sigma$.
	
La hipótesis a probar, es equivalente a probar:
	
$\begin{cases} H_{ 0 }:\quad \quad \boldsymbol{\mu_{D}}=0 \\ H_a: \quad \quad \boldsymbol{\mu_{D}} \neq 0  \end{cases}$
	
Bajo $H_0$ cierta el estadístico de prueba a usar es:
	
$T^{2}=n\boldsymbol{\bar{D}}^{t}{S}_{D}^{-1} \boldsymbol{\bar{D}} \sim \frac{\left(n-1\right)p}{n-p}{F}_{p;n-p}$
	
en donde, 
	
$\boldsymbol{\bar{D}}=\frac{1}{n}\sum_{i=1}^{n}\boldsymbol{D_i}$ y $S_{D}=\frac{1}{n-1}\sum_{i=1}^{n}\left(\boldsymbol{D_i}-\boldsymbol{\bar{D}}\right)\left(\boldsymbol{D_i}-\boldsymbol{\bar{D}}\right)$
	
son el vector de medias y la matriz de varianzas-covarianzas muestrales.
	
Se rechaza $H_0$ si ${T}_{0}^{2}>\frac{\left(n-1\right)p}{n-p}F_{p;n-p}$