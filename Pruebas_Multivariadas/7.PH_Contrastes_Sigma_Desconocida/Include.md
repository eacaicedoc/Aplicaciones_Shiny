#Prueba de Hipótesis Acerca de Contrastes para el vector de medias poblacional $\boldsymbol{\mu}$, de	una $N_p(\boldsymbol{\mu},\boldsymbol{\Sigma})$
	
#$\boldsymbol{\Sigma}$-Desconocida
	
Sea $\boldsymbol{X_1},\boldsymbol{X_2},...,\boldsymbol{X_n}$ una muestra aleatoria de una población p-variada con vector de medias $\boldsymbol{\mu}$-desconocido y matriz de varianza-covarianzas $\boldsymbol{\Sigma}$-desconocida, es decir $\boldsymbol{X_i} \sim N_p(\boldsymbol{\mu},\boldsymbol{\Sigma})$

	
Sea $C_{k*p}$ una matriz de constantes. C-contiene los coeficientes para k-combinaciones lineales simultaneas de las componentes de $\boldsymbol{\mu}={(\mu_1,\mu_2,...,\mu_p)}^{t}$ es decir:
	
$C\boldsymbol{\mu}=\begin{bmatrix} { c }_{ 11 } & { c }_{ 12 } & \cdots  & { c }_{ 1p } \\ { c }_{ 21 } & { c }_{ 22 } & \cdots  & { c }_{ 2p } \\ \vdots  & \vdots  & \vdots  & \vdots  \\ { c }_{ k1 } & { c }_{ k2 } & \cdots  & { c }_{ kp } \end{bmatrix}\begin{bmatrix} { \mu  }_{ 1 } \\ { \mu  }_{ 2 } \\ \vdots  \\ { \mu  }_{ p } \end{bmatrix}=\begin{bmatrix} c_{ 11 }\mu _{ 1 }+c_{ 12 }\mu _{ 2 }+\cdots +c_{ 1p }\mu _{ p } \\ c_{ 21 }\mu _{ 1 }+c_{ 22 }\mu _{ 2 }+\cdots +c_{ 2p }\mu _{ p } \\  \\ c_{k1}\mu_1+c_{k2}\mu_2+\cdots +c_{kp}\mu_p \end{bmatrix}$

	
Se desea contrastar las hipóteisis:
	
	
$\begin{cases} H_{ 0 }\quad \quad C\mu =\gamma  \\ H_{ 0 }\quad \quad C\mu \neq \gamma  \end{cases}\quad \quad \quad con\quad \quad \gamma=\begin{pmatrix} \gamma_{1}   \\ \gamma_{2}  \\ \vdots  \\  \gamma_{k} \end{pmatrix}$, vector de constantes 
	
Un estimador insesgado para $C\mu$ es $C\bar{\boldsymbol{X}}$, el cual tiene la siguiente distribución:
	
$C\bar{\boldsymbol{X}} \sim N_{k}(C\boldsymbol{\mu}, C\Sigma_{\boldsymbol{\bar{X}}}C^{t})$ es decir
	
$C\bar{\boldsymbol{X}} \sim N_{k}(C\boldsymbol{\mu}, \frac{1}{n}C\Sigma_{\boldsymbol{\bar{X}}}C^{t})$, pues $\Sigma_{\boldsymbol{\bar{X}}}=\frac{\Sigma}{n}$
	
Como $\Sigma$ es desconocida se usa el estadístico de prueba:
	
${T}_{0}^{2}=n\left(C\boldsymbol{\bar{X}}-\gamma\right)^{t}\left[CSC^{t}\right]^{-1}\left(C\boldsymbol{\bar{X}}-\gamma\right) \sim \frac{\left(n-1\right)k}{n-k}F_{k;n-k}$
	
Se rechaza $H_o$ si  ${T}_{o}^{2}>\frac{\left(n-1\right)k}{n-k}F_{k;n-k}$
	
#$\boldsymbol{\Sigma}$-Conocida

Sea $\boldsymbol{X_1},\boldsymbol{X_2},...,\boldsymbol{X_n}$ una muestra aleatoria de una población p-variada con vector de medias $\boldsymbol{\mu}$-desconocido y matriz de varianza-covarianzas $\boldsymbol{\Sigma}$-desconocida, es decir $\boldsymbol{X_i} \sim N_p(\boldsymbol{\mu},\boldsymbol{\Sigma})$

	
Sea $C_{k*p}$ una matriz de constantes. C-contiene los coeficientes para k-combinaciones lineales simultaneas de las componentes de $\boldsymbol{\mu}={(\mu_1,\mu_2,...,\mu_p)}^{t}$ es decir:
	
$C\boldsymbol{\mu}=\begin{bmatrix} { c }_{ 11 } & { c }_{ 12 } & \cdots  & { c }_{ 1p } \\ { c }_{ 21 } & { c }_{ 22 } & \cdots  & { c }_{ 2p } \\ \vdots  & \vdots  & \vdots  & \vdots  \\ { c }_{ k1 } & { c }_{ k2 } & \cdots  & { c }_{ kp } \end{bmatrix}\begin{bmatrix} { \mu  }_{ 1 } \\ { \mu  }_{ 2 } \\ \vdots  \\ { \mu  }_{ p } \end{bmatrix}=\begin{bmatrix} c_{ 11 }\mu _{ 1 }+c_{ 12 }\mu _{ 2 }+\cdots +c_{ 1p }\mu _{ p } \\ c_{ 21 }\mu _{ 1 }+c_{ 22 }\mu _{ 2 }+\cdots +c_{ 2p }\mu _{ p } \\  \\ c_{k1}\mu_1+c_{k2}\mu_2+\cdots +c_{kp}\mu_p \end{bmatrix}$


Se desea contrastar las hipóteisis:
	
$\begin{cases} H_{ 0 }\quad \quad C\mu =\gamma  \\ H_{ 0 }\quad \quad C\mu \neq \gamma  \end{cases}\quad \quad \quad con\quad \quad \gamma=\begin{pmatrix} \gamma_{1}   \\ \gamma_{2}  \\ \vdots  \\  \gamma_{k} \end{pmatrix}$, vector de constantes 
	
Un estimador insesgado para $C\mu$ es $C\bar{\boldsymbol{X}}$, el cual tiene la siguiente distribución:
	
$C\bar{\boldsymbol{X}} \sim N_{k}(C\boldsymbol{\mu}, C{\Sigma}C^{t})$ 
	
Como $\Sigma$ es conocida se usa el estadístico de prueba:
	
${\chi}_{0}^{2}=n\left(C\boldsymbol{\bar{X}}-\gamma\right)^{t}\left[C\Sigma C^{t}\right]^{-1}\left(C\boldsymbol{\bar{X}}-\gamma\right) \sim \chi_{k}^{2}$
	
Se rechaza $H_{0}$ si ${\chi}_{0}^{2}>\chi_{\alpha;k}^{2}$