## Prueba de hipótesis acerca de dos vectores de Medias Poblacionales $\boldsymbol\mu_1$ y $\boldsymbol\mu_2$, para Poblaciones Normales.

Sea ${\boldsymbol{X}}_{1}$,${\boldsymbol{X}}_{2}$,...,${\boldsymbol{X}}_{n}$ una m.a de una población normal p-variada con vector de medias ${\boldsymbol{\mu}}_{1}$- desconocida y matriz de varianzas-covarianzas ${\boldsymbol{\Sigma}}_{1}$, es decir $\boldsymbol{X_i}\sim N_p(\boldsymbol\mu_1, \boldsymbol\Sigma_1)$ y sean ${\boldsymbol{Y}}_{1}$,${\boldsymbol{Y}}_{2}$,...,${\boldsymbol{Y}}_{m}$ una m.a de una población normal p-variada con vector de medias ${\boldsymbol{\mu}}_{1}$-desconocida y matriz de varianzas-covarianzas ${\boldsymbol{\Sigma}}_{2}$, es decir $\boldsymbol{Y_i}\sim N_p(\boldsymbol\mu_2, \boldsymbol\Sigma_2)$. Ambas m.a son independientes entre si.
\
\


###Caso 1 : ${\boldsymbol{\Sigma}}_{1}$=${\boldsymbol{\Sigma}}_{2}$=${\boldsymbol{\Sigma}}-Conocidas$

\
Se desea contrastar las hipótesis

${H}_{0}$: ${\boldsymbol{\mu}}_{1}={\boldsymbol{\mu}}_{2}$

${H}_{a}$: ${\boldsymbol{\mu}}_{1}\neq{\boldsymbol{\mu}}_{2}$

Como ambas m.a provienen de poblaciones normales p-variadas, entonces:

$\bar{\boldsymbol{X}}\sim N_p({\boldsymbol{\mu}}_{1},\frac{\boldsymbol{\Sigma_1}}{n})$
\
$\bar{\boldsymbol{Y}}\sim N_p({\boldsymbol{\mu}}_{2},\frac{\boldsymbol{\Sigma_2}}{m})$
\
y como además, ambas m.a provienen de poblaciones independientes, luego:

$(\bar{\boldsymbol{x}}-\bar{\boldsymbol{y}})$ $\sim$ ${N}_{p}$  $\left(\boldsymbol{\mu}_{1}-\boldsymbol{\mu}_{2}, \frac{\boldsymbol\Sigma}{n}+\frac{\boldsymbol\Sigma}{m}\right)$
\

es decir

$Var[\bar{\boldsymbol{x}}-\bar{\boldsymbol{y}}]$=$\left(\frac{1}{n}+\frac{1}{m}\right)\boldsymbol\Sigma=\frac{n+m}{nm}\boldsymbol\Sigma$
\
Luego, bajo ${H}_{0}$: ${\boldsymbol{\mu}}_{1}={\boldsymbol{\mu}}_{2}$-cierto,es decir, ${H}_{0}$: ${\boldsymbol{\mu}}_{1}- {\boldsymbol{\mu}}_{2}=0$, 

\
se tiene que el estadístico de prueba:

$\chi_{0}^{2}=\left(\frac{nm}{n+m}\right)(\bar{\boldsymbol{x}}-\bar{\boldsymbol{y}})^t\boldsymbol\Sigma^{-1}(\bar{\boldsymbol{x}}-\bar{\boldsymbol{y}}) \sim \chi_{0}^{2}$
\
La regla de decisión es: Rechazamos $H_0$ si $\chi_{0}^{2}>\chi_{\alpha;p}^{2}$
\
\


###Caso 2 : ${\boldsymbol{\Sigma}}_{1}$=${\boldsymbol{\Sigma}}_{2}$=${\boldsymbol{\Sigma}}-Desconocidas$

En este caso de pruebas de hipótesis, la matriz ${\boldsymbol{\Sigma}}$ es igual pero desconocida por tanto tenemos similarmente:

${H}_{0}$: ${\boldsymbol{\mu}}_{1}={\boldsymbol{\mu}}_{2}$

${H}_{a}$: ${\boldsymbol{\mu}}_{1}\neq{\boldsymbol{\mu}}_{2}$


En este caso, se usa como estimador de $\Sigma$ a la varianza ponderada dada por:

$S_p=\hat{\boldsymbol\Sigma}=\frac{(n-1)S_1+(m-1)S_2}{n+m-2}$


Bajo $H_0$-cierto, el estadístico de prueba es:


$T^{2}=\frac{nm}{n+m}(\bar{\boldsymbol{x}}-\bar{\boldsymbol{y}}-\delta_0)^tS_{p}^{-1}(\bar{\boldsymbol{x}}-\bar{\boldsymbol{y}}-\delta_0) \sim \frac{(n+m-2)p}{n+m-p-1}F_{p;n+m-p-1}$


Rechazamos $H_0$ si $T^{2}>\frac{(n+m-2)p}{n+m-p-1}F_{p;n+m-p-1}$	
\


###Caso 3: $\boldsymbol\Sigma_1\neq\boldsymbol\Sigma_2$-Desconocida

En este caso de pruebas de hipótesis, las matrices ${\boldsymbol{\Sigma}}$ son desconocidas y diferentes por tanto tenemos similarmente:
\

Se desea contrastar las hipótesis:

${H}_{0}$: ${\boldsymbol{\mu}}_{1}- {\boldsymbol{\mu}}_{2}= 0$

${H}_{a}$: ${\boldsymbol{\mu}}_{1}- {\boldsymbol{\mu}}_{2}\neq 0$

\
Bajo $H_O$-cierto, el estadístico de prueba es:

$T^{2}=(\bar{\boldsymbol{x}}-\bar{\boldsymbol{y}}-\delta_0)^t[\frac{S_1}{n}+\frac{S_2}{m}](\bar{\boldsymbol{x}}-\bar{\boldsymbol{y}}-\delta_0) \sim \frac{vp}{v-p+1}F_{p;v-p+1}$

\
con 
\	
$v=\frac{tr(Se)+[tr(Se)]^{2}}{\sum_{i=i}^{2}\frac{1}{n_{i}-1}(tr(V_i)+[tr(V_i)]^{2})}$
\
$V_i=\frac{S_i}{n_i}$
\
$S_e=V_1+V_2=\frac{S_1}{n}+\frac{S_2}{m}$
\
\
Rechazamos $H_0$ si $T_{0}^{2}>\frac{vp}{v-p+1}F_{p;v-p+1}$

