# Type System

## Syntax

- Terms
  $$
  \begin{align*}
  t \coloneqq x &| t \texttt{\textasciitilde} t | \texttt{} \texttt{(}t\texttt{,}t\texttt{)} | \texttt{Left(}t\texttt{)} | \texttt{Right(}t\texttt{)}|t\&t|\texttt{<}t\texttt{,}t\texttt{>}\\
  &|1|\texttt{T}|\texttt{F}\\
  &|\texttt{(}x\texttt{,}x\texttt{)=}t\texttt{;}t|\texttt{case(}t\texttt{)}\texttt{\{Left(}x\texttt{):}t\texttt{,Right(}x\texttt{):}t\texttt{\}}\\
  &|t\texttt{.first}|t\texttt{.second}|\texttt{<}x\texttt{,}x\texttt{>=}t\texttt{;}t\\
  &|\texttt{one }t\texttt{;}t|\texttt{false }t\texttt{;}t|\texttt{zero }t
  \end{align*}
  $$

## Types & Environments

- Types
  $$
  \sigma, \tau \coloneqq 1 | 0 | \top | \bot | \tau \otimes \tau | \tau \oplus \tau | \tau \& \tau | \tau ⅋ \tau
  $$
- Environments
  $$
  \Gamma, \Delta \coloneqq \emptyset | \Gamma, x : \tau
  $$
- Hyper Environments
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
(\sigma \oplus \tau)^{-1} &\coloneqq \sigma^{-1} \& \tau^{-1}\\
(\sigma \& \tau)^{-1} &\coloneqq \sigma^{-1} \oplus \tau^{-1}\\
(\sigma ⅋ \tau)^{-1} &\coloneqq \sigma^{-1} \otimes \tau^{-1}
\end{align*}
$$

### Primitive Rules

- var
  $$
  \frac{}{x : \tau \vdash x : \tau}
  $$
- cut
  $$
  \frac{\Gamma_1 \vdash t_1 : \sigma \quad \Gamma_2 \vdash t_2 : \sigma^{-1} \quad \Phi \vdash t_3 : \tau}{\Phi;\Gamma_1, \Gamma_2 \vdash t_1 \texttt{\textasciitilde} t_2 \texttt{;} t_3 : \tau}
  $$

### Composition Rules

- $\otimes$
  $$
  \frac{\Gamma_1 \vdash t_1 : \tau_1 \quad \Gamma_2 \vdash t_2 : \tau_2}{\Gamma_1, \Gamma_2 \vdash \texttt{(}t_1\texttt{,}t_2\texttt{)} : \tau_1 \otimes \tau_2}
  $$
- $\oplus$ (Left)
  $$
  \frac{\Phi\vdash t : \tau_1}{\Phi \vdash \texttt{Left(}t\texttt{)} : \tau_1 \oplus \tau_2}
  $$
- $\oplus$ (Right)
  $$
  \frac{\Phi\vdash t : \tau_2}{\Phi \vdash \texttt{Right(}t\texttt{)} : \tau_1 \oplus \tau_2}
  $$
- $\&$
  $$
  \frac{\Phi\vdash t_1 : \tau_1 \quad \Phi\vdash t_2 : \tau_2}{\Phi \vdash t_1 \& t_2 : \tau_1 \& \tau_2}
  $$
- $⅋$
  $$
  \frac{\Phi\vdash t_1 : \tau_1 \quad \Psi\vdash t_2 : \tau_2}{\Phi;\Psi \vdash \texttt{<}t_1 \texttt{,}t_2\texttt{>} : \tau_1 ⅋ \tau_2}
  $$
- $1$
  $$
  \frac{}{\vdash 1 : 1}
  $$
- $\top$
  $$
  \frac{}{\Phi \vdash \top : \top}
  $$
- $\bot$
  $$
  \frac{\Phi;\bot : \bot \vdash t : \tau}{\Phi \vdash \bot : \tau}
  $$
