---
title: "Probaibilidades"
runtime: shiny
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# DISTRIBUIÇÃO DE BERNOULLI

Determinados experimentos são tais que os resultados apresentam ou não determianda característica. Por exemplo:

* em um determinado jogo temos apenas dois resultados possíveis: vitória ou derrota;

* uma pessoa escolhida em um grupo de $200$ pessoas é ou não estudante;

* uma peça é escolhida dentro de um lote de $100$ peças, podendo ela ser ou não defeituosa;

* no lançamento de um dado podemos obter a face $1$ ou não (neste caso, teremos as faces $2,3,4,5$ ou $6$).

Em todas as situações descritas acima estamos interessados em obter algo (vamos chamá-lo de Sucesso) ou não obtê-lo (chamaremos de Fracasso).

Podemos assim definir uma variável aleatória X que pode assumir apenas dois valores: 1, se ocorrer sucesso, e 0, se ocorrer fracasso. Indicaremos por p a probabildade de sucesso, isto é: P(Sucesso)= $P(S)=p$, $0<p<1$.

Def.: A variável aleatória X, que assume apenas os valores $0$ e $1$, com função de probabilidade $(x, p(x))$ tal que:

$p(0)=P(X=0)=1-p$
$p(1)=P(X=1)=p$

é chamada de variável aleatória de Bernoulli.


# DISTRIBUIÇÃO BINOMIAL

Suponha um ensaio de Bernoulli repetido n vezes. Considere também que as repetições são independentes, ou seja, o resultado de um ensaio não tem influência nenhuma no resultado de outro ensaio. Alternativamente, podemos representar uma sequência de sucessos e fracassos com uns e zeros. Por exemplo, ao repetirmos um ensaio de Bernoulli 4 vezes, um dos resultados pode ser SFFS ou (1,0,0,1). Logo, a probabilidade de tal amostra será:

$p(1-p)(1-p)p=p^{2}(1-p)^{2}$

Um aspecto importante a ser observado nesse tipo de experimento é que estamos interessados apenas no número total de sucessos e, não na ordem em que eles ocorrem. Considere, por exemplo, o lançamento de uma moeda $3$ vezes ($n=3$), com $P(S)=p$, $P(F)=1-p=q$

Utilize o App Shiny abaixo e faça as simulações, considerando todos os cenários possíveis para a situação descrita.

```{r}
library(shiny)

ui<-fluidPage(
  pageWithSidebar(
    headerPanel("DISTRIBUIÇÃO BINOMIAL DE PROBABILIDADES"),
    
    sidebarPanel(
    
    sliderInput("n", "Selecione o número de repetições (n):",
                min = 1,max = 50, value = 1),
    sliderInput("p", "Selecione a Probabilidade de Sucesso (p):",
                min = 0, max = 1, value = 0.5, step=0.01)
    
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("grafico",plotOutput("grafico")),
      tabPanel("tabela", tableOutput("tabela"))
    )
  
    
  )
  
)
)
server<-function(input,output){
  
  output$grafico<-renderPlot({
    tipo<-0:input$n
    tipo1<-input$n
    tipo2<-input$p
    
   plot(tipo,dbinom(tipo,tipo1,tipo2),col="black", pch=16, xlab = "Valor", main= "DISTRIBUIÇÃO DE PROBABILIDADE",
        ylab="Probabilidade",bty="n", type = 'h')
  })
  
  output$tabela<-renderTable({
    Sucessos<-0:input$n
    tipo1<-input$n
    tipo2<-input$p
    
    fórmula<-function(Sucessos,tipo1,tipo2){
      Probabilidade<-dbinom(Sucessos,tipo1,tipo2)
      round(Probabilidade,digits=4)
      teste<-data.frame(Sucessos,Probabilidade)
      
      
      
    }
    
    fórmula(Sucessos,tipo1,tipo2)
    
    })
  }
shinyApp(ui=ui,server = server)
```

Considere um experimento que se repete $n$ vezes, com exatamente $k$ sucessos. Tal probabilidade pode ser calculada pela expressão: 

$P(X=k)=C_{n,k}p^{k}q^{n-k}$, onde:

* $p$ é a probabilidade de sucesso;

* $q=1-p$ e´a probabilidade de fracasso.


## Exemplo:

Lançando-se uma moeda não viciada¨$6$ vezes, qual a probabilidade de se obter:

$a)$ exatamente $3$ caras?

$b)$ pelo menos $2$ caras?

Resolução: $a)$

```{r}
k=3 #numero de sucessos
n=6 #tentativas
p=0.5 #probabilidade de sucesso
probability=dbinom(k,n,p)
print(probability)

```

$b)$

```{r}
k=1 # considerando o acumulado para 0 e 1 lançamentos
n=6 #tentativas
p=0.5 #probabilidade de sucesso
probability_acum=pbinom(k,n,p)
probability_acum #acumulado para nenhuma cara ou 1 cara
result=1-probability_acum #resultado da probabilidade acumulada para valores maiores ou iguais a 2

result
```


# DISTRIBUIÇÃO DE PASCAL

A distribuição de Pascal ou Binomial Negativa, envolve o desejo de se conhecer a probabilidade para que determinado experimento tenha exatamente $r$ sucessos após $x$ repetições, ou seja: 

$P(X=x)=C_{x-1,r-1}p^{r}q^{x-r}$, onde:

* $p$ é a probabilidade de sucesso;

* $q=1-p$ é a probabilidade de fracasso

## Exemplo:

$1.$ Suponha que a probabilidade dos sinais de trânsito estarem abertos seja $0.3$. Qual a probabilidade de, após passar por $10$ sinais, exatamente $4$ deles estarem abertos?

```{r}
f=6 #número de fracassos;
r=4 #número de sucessos;
p=0.3 #probabilidade de sucesso
dnbinom(f,r,p)
```

Vamos utilizar o App shiny para analisar o comportamento desse tipo de distribuição:

```{r}
library(shiny)

ui<-fluidPage(
  pageWithSidebar(
    headerPanel("DISTRIBUIÇÃO DE PASCAL OU BINOMIAL NEGATIVA"),
    
    sidebarPanel(
    
    sliderInput("n", "Selecione o número de tentativas (n):",
                min = 1,max = 50, value = 1),
    sliderInput("f", "Selecione o número de fracassos (f):",
                min = 1,max = 50, value = 1),
    sliderInput("p", "Selecione a Probabilidade de Sucesso (p):",
                min = 0, max = 1, value = 0.5, step=0.01)
    
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("grafico",plotOutput("grafico")),
      tabPanel("tabela", tableOutput("tabela"))
    )
  
    
  )
  
)
)
server<-function(input,output){
  
  output$grafico<-renderPlot({
    tipo<-0:input$n
    tipo1<-input$f
    tipo2<-input$p
    
   plot(tipo,dnbinom(tipo1,tipo,tipo2),col="black", pch=16, xlab = "Valor", main= "DISTRIBUIÇÃO DE PROBABILIDADE",
        ylab="Probabilidade",bty="n", type = 'h')
  })
  
  output$tabela<-renderTable({
    tipo<-0:input$n
    tipo1<-input$f
    tipo2<-input$p
    
    fórmula<-function(tipo,tipo1,tipo2){
      Probabilidade<-dnbinom(tipo1,tipo,tipo2)
      round(Probabilidade,digits=4)
      teste<-data.frame(tipo,Probabilidade)
      
      
      
    }
    
    fórmula(tipo, tipo1, tipo2)
    
    })
  }
shinyApp(ui=ui,server = server)
```



