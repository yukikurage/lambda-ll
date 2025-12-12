# Type System

## Syntax

**Terms**

$$
  \begin{align*}
  t \coloneqq &x | \texttt{(}t\texttt{,}t\texttt{)} | \texttt{left(}t\texttt{)} | \texttt{right(}t\texttt{)}|t\And t|\texttt{$\lbrace$}t\texttt{,}t\texttt{$\rbrace$}\\
  &|\texttt{()}|\texttt{drop(}t\texttt{)}|\texttt{$\lbrace\rbrace$} \\ 
  &|\texttt{case(}t\texttt{)}\texttt{[left(}x\texttt{)->}t\texttt{,right(}x\texttt{)->}t\texttt{]}
  |t\texttt{.first}|t\texttt{.second}\\
  &|\texttt{abort(}t\texttt{)}\\
  &|\texttt{$\lbrace$}s;t\texttt{$\rbrace$}
  \end{align*}
$$

**Statement**

$$
  \begin{align*}
  s^1 \coloneqq & x\ \texttt{=}\ t  \\
  &|x\ \texttt{<->}\ y | t\ \texttt{>-<}\ t \\
  &|\texttt{(}x\texttt{,}x\texttt{)}\ \texttt{=}\ t|\texttt{$\lbrace$}x\texttt{,}x\texttt{$\rbrace$}\ \texttt{=}\ t\\
  &|\texttt{<-}\ t|\texttt{->}\ t
  \end{align*}
$$

**Statement Sequence**

$$
s \coloneqq s^1; s^1; \cdots; s^1
$$

## Types & Environments

**Types**

$$
  \sigma, \tau \coloneqq 1 | 0 | \top | \bot | \tau \otimes \tau | \tau \oplus \tau | \tau \And  \tau | \tau ⅋ \tau
$$

**Environment**

$$
  \Gamma \coloneqq \emptyset | \Gamma, x : \tau
$$

**Result**

$$
  \Delta \coloneqq \emptyset | \Delta, t : \tau
$$

## Type Judgments

**Judgement Form**

$$
s; \Gamma \vdash \Delta
$$

(we can freely permute environment $\Gamma$ and result $\Delta$)

**Type Inversion**

$$
\begin{align*}
1^{-1} &\coloneqq \bot\\
0^{-1} &\coloneqq \top\\
\top^{-1} &\coloneqq 0\\
\bot^{-1} &\coloneqq 1\\
(\sigma \otimes \tau)^{-1} &\coloneqq \sigma^{-1} ⅋ \tau^{-1}\\
(\sigma \oplus \tau)^{-1} &\coloneqq \sigma^{-1} \And  \tau^{-1}\\
(\sigma \And  \tau)^{-1} &\coloneqq \sigma^{-1} \oplus \tau^{-1}\\
(\sigma ⅋ \tau)^{-1} &\coloneqq \sigma^{-1} \otimes \tau^{-1}
\end{align*}
$$

**Statement Mixing**

Actually, meta variable $s$ in type judgement represents preordered set of statements.

$$
s \cup s' = \text{All statements s.t. keeping the order of statements in $s$ and $s'$}
$$

Ex.

$$
x = t; y = u; \quad  \in s
$$

$$
z = a; w = b; \quad \in s'
$$

then...

$$
x = t; y = u; z = a; w = b;\quad  \in s \cup s'
$$

also...

$$
z = a;  x = t; w = b; y = u;\quad  \in s \cup s'
$$

### Structural Rules

**variable introduction**

$$
\frac{}{;x : \tau \vdash x : \tau}
$$

**transitivity**

$$
\frac{s_1;\Gamma_1 \vdash x : \tau, \Delta_1 \quad s_2; \Gamma_2, x : \tau \vdash \Delta_2}{s_1 \cup s_2; \Gamma_1, \Gamma_2 \vdash \Delta_1, \Delta_2}
$$

### Syntactic Rules

**variable binding**

$$
\frac{s;\Gamma \vdash t : \tau, \Delta}{s; x = t; \Gamma \vdash x : \tau, \Delta}
$$

**scoping**

$$
\frac{s;\Gamma \vdash t : \tau}{; \Gamma \vdash \lbrace s; t \rbrace : \tau}
$$

### Primitive Rules

**initialization**

$$
\frac{}{x\ \texttt{<->}\ y;\vdash x : \tau, y : \tau^{-1}}
$$

**cut**

$$
\frac{s_1;\Gamma_1 \vdash t_1 : \tau, \Delta_1 \quad s_2; \Gamma_2 \vdash t_2 : \tau^{-1}, \Delta_2}{s_1 \cup s_2; t_1\ \texttt{>-<}\ t_2 ; \Gamma_1, \Gamma_2 \vdash \Delta_1, \Delta_2}
$$

### Introduction Rules

**tensor introduction**

$$
\frac{s_1;\Gamma_1 \vdash t_1 : \tau_1, \Delta_1 \quad s_2;\Gamma_2 \vdash t_2 : \tau_2, \Delta_2}{s_1 \cup s_2; \Gamma_1, \Gamma_2 \vdash \texttt{(}t_1\texttt{,}t_2\texttt{)} : \tau_1 \otimes \tau_2, \Delta_1, \Delta_2}
$$

**plus introduction** (Left)

$$
\frac{s ;\Gamma \vdash t : \tau_1, \Delta}{s ;\Gamma \vdash \texttt{left(}t\texttt{)} : \tau_1 \oplus \tau_2, \Delta}
$$

**plus introduction** (Right)

$$
\frac{s ;\Gamma \vdash t : \tau_2, \Delta}{s ;\Gamma \vdash \texttt{right(}t\texttt{)} : \tau_1 \oplus \tau_2, \Delta}
$$

**with introduction**

$$
\frac{s; \Gamma \vdash t_1 : \tau_1, \Delta \quad s; \Gamma \vdash t_2 : \tau_2, \Delta}{s; \Gamma \vdash t_1 \And t_2 : \tau_1 \And \tau_2, \Delta}
$$

**par introduction**

$$
\frac{s;\Gamma \vdash t_1 : \tau_1, t_2 : \tau_2, \Delta}{s;\Gamma \vdash \lbrace t_1 \texttt{,}t_2 \rbrace : \tau_1 ⅋ \tau_2, \Delta}
$$

**unit introduction**

$$
\frac{}{;\vdash \texttt{()} : 1}
$$

**top introduction**

$$
\frac{s;\Gamma \vdash t : \tau, \Delta}{s;\Gamma \vdash \texttt{drop(}t\texttt{)} : \top, \Delta}
$$

**bottom introduction**

$$
\frac{s;\Gamma\vdash\Delta}{s;\Gamma\vdash \lbrace\rbrace : \bot, \Delta}
$$

### Elimination Rules

**tensor elimination**

$$
\frac{s;\Gamma\vdash t:\tau_1 \otimes\tau_2, \Delta \quad s';\Gamma, x : \tau_1, y : \tau_2 \vdash \Delta'}{s; (x, y) = t; s'; \Gamma\vdash \Delta, \Delta'}
$$

**plus elimination**

$$
\frac{s;\Gamma\vdash t : \tau_1 \oplus \tau_2, \Delta \quad ;\Gamma', x: \tau_1 \vdash t_1 : \tau' \quad ;\Gamma', y: \tau_2 \vdash t_2 : \tau'}{s; \Gamma, \Gamma' \vdash \texttt{case(}t\texttt{)[left(}x\texttt{)->}t_1\texttt{,right(}y\texttt{)->}t_2\texttt{]} : \tau', \Delta}
$$

**with elimination** (First)

$$
\frac{s; \Gamma \vdash t : \tau_1 \And \tau_2, \Delta}{s; \Gamma \vdash \texttt{first(}t\texttt{)} : \tau_1, \Delta}
$$

**with elimination** (Second)

$$
\frac{s; \Gamma \vdash t : \tau_1 \And \tau_2, \Delta}{s; \Gamma \vdash \texttt{second(}t\texttt{)} : \tau_2, \Delta}
$$

**par elimination**

$$
\frac{s; \Gamma \vdash t : \tau_1 ⅋ \tau_2, \Delta}{s; \lbrace x, y \rbrace = t; \Gamma \vdash x : \tau_1, y : \tau_2, \Delta}
$$

**unit elimination**

$$
\frac{s_1; \Gamma_1 \vdash t_1 : 1, \Delta_1 \quad s_2; \Gamma_2 \vdash \Delta_2}{(s_1; \texttt{<-} t_1) ∪ s_2; \Gamma_1, \Gamma_2 \vdash \Delta_1, \Delta_2}
$$

**void elimination**

$$
\frac{s;\Gamma \vdash t : 0, \Delta}{s;\Gamma \vdash \texttt{abort(}t\texttt{)} : \tau, \Delta}
$$

**bottom elimination**

$$
\frac{s; \Gamma \vdash t : \bot, \Delta}{s; \texttt{->} t; \Gamma \vdash \Delta}
$$

### Exponential Rules

**UNDER CONSTRUCTION**
