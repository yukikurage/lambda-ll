# Type System

## Syntax

**Terms**

$$
  \begin{align*}
  t \coloneqq x &| t \sim t | \mathtt{(}t\mathtt{,}t\mathtt{)} | \mathtt{Left(}t\mathtt{)} | \mathtt{Right(}t\mathtt{)}|t\And t|\mathtt{[}t\mathtt{,}t\mathtt{]}\\
  &|1|\mathtt{T}|\mathtt{F}\\
  &|\mathtt{(}x\mathtt{,}x\mathtt{)=}t\mathtt{;}t|\mathtt{case(}t\mathtt{)}\mathtt{\{Left(}x\mathtt{):}t\mathtt{,Right(}x\mathtt{):}t\mathtt{\}}\\
  &|t\mathtt{.first}|t\mathtt{.second}|\mathtt{[}x\mathtt{,}x\mathtt{]=}t\mathtt{;}t\\
  &|\mathtt{one}\ t\mathtt{;}t|\mathtt{bottom}\ t\mathtt{;}t|\mathtt{zero}\ t
  \end{align*}
$$

## Types & Environments

**Types**

$$
  \sigma, \tau \coloneqq 1 | 0 | \top | \bot | \tau \otimes \tau | \tau \oplus \tau | \tau \And  \tau | \tau ⅋ \tau
$$

**Environments**

$$
  \Gamma, \Delta \coloneqq \emptyset | \Gamma, x : \tau
$$

**Hyper Environments**

$$
  \Psi, \Phi \coloneqq \emptyset | \Psi ; \Gamma
$$

## Type Judgments

### Type Inversion

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

### Primitive Rules

**var**

$$
\frac{}{x : \tau \vdash x : \tau}
$$

**cut**

$$
\frac{\Gamma_1 \vdash t_1 : \sigma \quad \Gamma_2 \vdash t_2 : \sigma^{-1} \quad \Phi \vdash t_3 : \tau}{\Phi;\Gamma_1, \Gamma_2 \vdash t_1 \sim t_2 \mathtt{;} t_3 : \tau}
$$

### Composition Rules

**tensor**

$$
\frac{\Gamma_1 \vdash t_1 : \tau_1 \quad \Gamma_2 \vdash t_2 : \tau_2}{\Gamma_1, \Gamma_2 \vdash \mathtt{(}t_1\mathtt{,}t_2\mathtt{)} : \tau_1 \otimes \tau_2}
$$

**plus** (Left)

$$
\frac{\Phi\vdash t : \tau_1}{\Phi \vdash \mathtt{Left(}t\mathtt{)} : \tau_1 \oplus \tau_2}
$$

**plus** (Right)

$$
\frac{\Phi\vdash t : \tau_2}{\Phi \vdash \mathtt{Right(}t\mathtt{)} : \tau_1 \oplus \tau_2}
$$

**with**

$$
\frac{\Phi\vdash t_1 : \tau_1 \quad \Phi\vdash t_2 : \tau_2}{\Phi \vdash t_1 \And  t_2 : \tau_1 \And  \tau_2}
$$

**par**

$$
\frac{\Phi\vdash t_1 : \tau_1 \quad \Psi\vdash t_2 : \tau_2}{\Phi;\Psi \vdash \mathtt{[}t_1 \mathtt{,}t_2\mathtt{]} : \tau_1 ⅋ \tau_2}
$$

**one**

$$
  \frac{}{\vdash \mathtt{1} : 1}
$$

**top**

$$
  \frac{}{\Phi \vdash \mathtt{T} : \top}
$$

**bottom**

$$
  \frac{\Phi;\mathtt{F} : \bot \vdash t : \tau}{\Phi \vdash t : \tau}
$$
