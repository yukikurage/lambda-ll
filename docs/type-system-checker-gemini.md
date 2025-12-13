はい、その通りです。**Par (`⅋`) の導入も、LCA が Par ノードであることを確認する以外は、全く同じ「Zipper Merge（再帰的合併）」アルゴリズムで動作します。**

これまでの議論を整理し、実装のためのアルゴリズム仕様書として Markdown にまとめました。

-----

# 線形型言語 型検査器 実装仕様書

本システムは、ネストした環境（Gamma）を木構造として持ち、項（Term）の構築に伴ってその木構造を書き換えていく「状態遷移マシン」として実装する。

## 1\. データ構造

### 1.1 Gamma Tree (環境木)

環境は `Tensor` 層と `Par` 層が交互に現れる木構造である。

```rust
enum GammaNode {
    // 葉: 変数リソース
    Leaf {
        id: NodeId,
        ty: Type,
        name: String 
    },
    // 節: 環境の分割 (直積)
    Tensor {
        id: NodeId,
        children: Vec<NodeId> // 子は必ず Par ノード または Leaf
    },
    // 節: 環境の共有 (直和/並行)
    Par {
        id: NodeId,
        children: Vec<NodeId> // 子は必ず Tensor ノード または Leaf
    }
}

// 状態全体
struct CheckState {
    nodes: Map<NodeId, GammaNode>,
    root: NodeId,
}
```

### 1.2 正規化ルール (Invariants)

アルゴリズムを単純化するため、常に以下の状態を維持する。

1.  **交互層:** `Tensor` の子は `Par` (または葉)、`Par` の子は `Tensor` (または葉) である。
2.  **単一子の排除:** 子が1つしかない `Tensor` や `Par` は、その親に吸収（Flatten）させる。

-----

## 2\. 型検査のメインフロー

型検査は `State Monad` パターンを用いる。

  * **入力:** 現在の `CheckState`
  * **処理:** 文または項による書き換え
  * **出力:** 更新された `CheckState`（項の場合は + 生成されたノードID）

### 2.1 Statement の検査 (`checkStmt`)

```rust
fn check_stmt(stmt: Statement, state: &mut CheckState) -> Result<(), Error> {
    match stmt {
        // 変数束縛: x = t
        Statement::Bind(name, term) => {
            // 1. 右辺 t を評価し、Gammaを変形。生成された項の場所(id)を得る
            let (term_id, term_ty) = check_term(term, state)?;
            
            // 2. その場所に名前を付ける
            state.set_name(term_id, name);
            Ok(())
        },

        // 送信(Bottom Elim): -> t
        Statement::Send(term) => {
            let (id, ty) = check_term(term, state)?;
            if ty != Type::Bottom { return Err("Type Mismatch"); }
            
            // 3. Bottom型の項は「計算の終了/脱出」なので環境から削除する
            state.remove_node(id); 
            Ok(())
        },
        
        // ... 他の文
    }
}
```

-----

## 3\. Term の検査と「Zipper Merge」

ここがアルゴリズムの核となる。

### 3.1 `check_term` の概略

```rust
fn check_term(term: Term, state: &mut CheckState) -> Result<(NodeId, Type), Error> {
    match term {
        // 変数使用
        Term::Var(x) => {
            let id = state.find_node_by_name(x)?;
            let ty = state.get_type(id);
            // ※ここでは消費せず、IDを返すだけ。
            // 複数の変数を組み合わせる時に初めて移動/合併が起こる。
            Ok((id, ty))
        },

        // Tensor 導入: (t1, t2)
        Term::Tensor(t1, t2) => {
            // 1. 再帰的に評価 (Stateは更新される)
            let (id1, ty1) = check_term(*t1, state)?;
            let (id2, ty2) = check_term(*t2, state)?;

            // 2. 結合操作 (Merge)
            // LCA が Tensor であることを要求
            let new_id = state.merge_nodes(id1, id2, NodeType::Tensor)?;
            
            Ok((new_id, Type::Tensor(ty1, ty2)))
        },

        // Par 導入: {t1, t2}
        Term::Par(t1, t2) => {
            let (id1, ty1) = check_term(*t1, state)?;
            let (id2, ty2) = check_term(*t2, state)?;

            // 2. 結合操作 (Merge)
            // LCA が Par であることを要求
            let new_id = state.merge_nodes(id1, id2, NodeType::Par)?;
            
            Ok((new_id, Type::Par(ty1, ty2)))
        }
    }
}
```

### 3.2 Merge アルゴリズム (Zipper Merge)

2つのノード `n1`, `n2` を統合し、新しいノードを作る操作。

**シグネチャ:**
`fn merge_nodes(&mut self, n1: NodeId, n2: NodeId, target_kind: NodeType) -> Result<NodeId, Error>`

**手順:**

1.  **LCA 探索:**
    `n1` と `n2` の最小共通祖先 (LCA) を探す。

      * もし `LCA.kind != target_kind` ならエラー（Tensor結合したいのにLCAがPar、など）。

2.  **パスの特定:**
    LCA の子ノードのうち、`n1` を含むものを `S`、`n2` を含むものを `T` とする。
    LCA のその他の子ノード（無関係なリソース）を `U` とする。

3.  **書き換え (Rewrite):**
    LCA の構造を以下のように変更する。
    $$LCA[S, T, U] \Longrightarrow LCA[\textbf{RecursiveMerge}(S, T), U]$$

4.  **Recursive Merge (再帰的合併):**
    `S` と `T` の構造（Par/Tensor）に応じて、中身を「ジッパー」のように噛み合わせる。

      * **Case 1: 同じ種類のノード同士の合併 (Par同士, Tensor同士)**
        例: `S = Par[A, B]`, `T = Par[D, E]` (Aにn1, Dにn2がいる)

          * ターゲットを含む子 `A` と `D` を再帰的に `RecursiveMerge(A, D)` する。
          * 残りの子 `B, E` は、そのまま並列に配置する。
          * 結果: `Par[ RecursiveMerge(A, D), B, E ]`
          * *(弱分配律によるコンテキストの共有)*

      * **Case 2: ターゲット到達 (n1 と n2 が直接出会った)**

          * `n1` と `n2` を削除し、その位置に新しい項 `NewTerm` を配置する。
          * 結果: `NewTerm`

### 3.3 具体的な書き換え例 (Tensor Intro)

**Before:**
$x, y$ の LCA は `Tensor`。

```text
      (LCA: Tensor)
      /           \
   (S: Par)     (T: Par)
   /      \     /      \
 (A: Tns)  B  (D: Tns)  E
    |            |
    x            y
```

**After:**
`merge_nodes(x, y, Tensor)` 実行後。

```text
      (LCA: Tensor)
          |
    (Result: Par)
    /     |     \
 (x,y)    B      E
```

  * `x` と `y` は結合されて `(x, y)` になる。
  * `S` と `T` (Par層) は合併し、文脈 `B` と `E` は共有される。

-----

## 4\. Additive (With / Case) の処理

分岐を扱うため、Gamma のスナップショット機能を利用する。

### 4.1 With Introduction (`t1 & t2`)

```rust
// t1 & t2
fn check_with(t1: Term, t2: Term, state: &mut CheckState) -> Result<...> {
    // 1. 状態の保存
    let snapshot = state.clone();

    // 2. 左の分岐を評価
    let (id1, ty1) = check_term(t1, state)?;
    let state_after_left = state.clone();

    // 3. 状態をロールバックして、右の分岐を評価
    *state = snapshot;
    let (id2, ty2) = check_term(t2, state)?;

    // 4. 整合性チェック (Verify)
    // - id1 と id2 が指すリソース(穴)の位置が論理的に同じか？
    // - 消費されたリソースの集合が一致しているか？
    verify_consistency(&state_after_left, state)?;
    
    // 5. 状態の確定
    // どちらかの状態を採用し(消費リソースは同じはずなので)、
    // 新しい With ノードを配置する。
    Ok((new_id, Type::With(ty1, ty2)))
}
```

-----

## 5\. まとめ

この実装方針のメリット：

1.  **Result ($\Delta$) 管理が不要:**
    項の生成は「木の葉を消費して、LCAの位置に新しいノードを生やす」操作として完結するため、環境木一本で管理できる。
2.  **論理的な正しさ:**
    LCA判定と Zipper Merge は、Proof Net の正当性判定（Correctness Criterion）と弱分配律（Weak Distributivity）を直接実装したものであり、バグが入りにくい。
3.  **エラーメッセージ:**
    「変数 a と b を Tensor したいけど、LCA が Par だからできない（文脈が絡まっている）」といった直感的なエラーが出せる。
