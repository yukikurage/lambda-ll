これまでの議論（Zipper Merge の導入と、Par/Tensor の非対称性の確認）を踏まえた、**Context Tree (Gamma) の最終的な要件定義書**です。

実装の際の「Source of Truth（信頼できる唯一の情報源）」として使えるようにまとめました。

---

# Context Tree (Gamma) 実装要件定義書

本ドキュメントは、線形型言語の型検査における環境（$\Gamma$）のデータ構造と、項（Term）構築に伴う操作アルゴリズムを定義する。

## 1\. データ構造定義

環境は、リソースの依存関係（直積・直和）を表す木構造である。

### 1.1 ノード構造

```rust
type NodeId = usize; // または UUID

enum GammaNode {
    /// リソース（変数）。木の葉 (Leaf) となる。
    Leaf {
        id: NodeId,
        name: String,
        ty: Type,
    },

    /// 直積 (Separate Contexts)。
    /// 異なるブランチのリソースは独立している。
    /// 子ノードは必ず Par または Leaf である (正規化条件)。
    Tensor {
        id: NodeId,
        children: Vec<NodeId>,
    },

    /// 並行/直和 (Shared Context)。
    /// 異なるブランチのリソースは互いに依存している (Entangled)。
    /// 子ノードは必ず Tensor または Leaf である (正規化条件)。
    Par {
        id: NodeId,
        children: Vec<NodeId>,
    }
}
```

### 1.2 正規化不変条件 (Invariants)

アルゴリズムを簡素化するため、Gamma は常に以下の状態を維持する。

1.  **交互層 (Alternating Layers):**
    - `Tensor` の直接の子は `Par` または `Leaf`。
    - `Par` の直接の子は `Tensor` または `Leaf`。
2.  **縮約 (Contraction):**
    - 子が 1 つだけの `Tensor` または `Par` ノードは存在しない。
    - もし操作の結果、子が 1 つになった場合は、即座にその親ノードにマージ（Flatten）される。
    - 子が 0 個になったノードは削除される。

---

## 2\. 共通ユーティリティ操作

### 2.1 LCA (Lowest Common Ancestor)

- **入力:** `id1`, `id2`
- **出力:** `NodeId` (最小共通祖先ノード)
- **要件:** 2 つのノードが合流する最も深いノードを特定する。

### 2.2 Path Check (パス検証)

- **入力:** `ancestor`, `target`
- **出力:** `bool` (経路に Tensor が含まれているか)
- **要件:** `ancestor` から `target` への経路を探索し、途中に `Tensor` ノードが存在するかを判定する。`target` 自身や `ancestor` 自身の種類は問わない（中間のパスのみ）。

---

## 3\. Tensor Merge (Zipper Algorithm)

`(t1, t2)` の導入時に実行される。環境の構造を書き換える（弱分配律の適用）。

### 3.1 前提条件

1.  **LCA Type:** LCA は `Tensor` ノードでなければならない。
    - _Error:_ LCA が `Par` の場合 → 「文脈が絡み合っているため分離できません」。

### 3.2 アルゴリズム

2 つのターゲット `x` (t1 由来), `y` (t2 由来) を結合する。

1.  **子ノードの特定:**
    LCA (`Tensor`) の子ノードのうち：

    - `x` を含むノードを $S$ とする（$S$ は `Par` または `Leaf`）。
    - `y` を含むノードを $T$ とする（$T$ は `Par` または `Leaf`）。
    - それ以外の子ノード群を $U$ とする。

2.  **トップレベル書き換え:**
    LCA の構造を以下のように変更する。

    $$
    \text{Tensor}[S, T, U] \Longrightarrow \text{Tensor}[\textbf{Zip}(S, T), U]
    $$

3.  **Zip (再帰的合併) 関数:**
    2 つのノード $A$ ($x$を含む), $B$ ($y$を含む) を結合する。

    - **Case 1: $A, B$ が共に `Par` の場合**

      - $A$ の子で $x$ を含むものを $A'$、残りを $RestA$。
      - $B$ の子で $y$ を含むものを $B'$、残りを $RestB$。
      - **Action:** 新しい `Par` を作成し、文脈を共有させる。

      $$
      \textbf{Zip}(A, B) \rightarrow \text{Par}[ \textbf{Zip}(A', B'), RestA..., RestB... ]
      $$

    - **Case 2: $A, B$ が共に `Tensor` の場合**

      - $A$ の子で $x$ を含むものを $A'$、残りを $RestA$。
      - $B$ の子で $y$ を含むものを $B'$、残りを $RestB$。
      - **Action:** 新しい `Tensor` を作成し、文脈を分離したままにする。

      $$
      \textbf{Zip}(A, B) \rightarrow \text{Tensor}[ \textbf{Zip}(A', B'), RestA..., RestB... ]
      $$

    - **Case 3: $A$ が `Par` で、$B$ が `Tensor` の場合**

      - 右辺を $B$ を一つのみ含む `Par` の Singleton に変換し、続ける。

    - **Case 3+:** その他の不整合も同様に処理する。

    - **Case 4: $x$ と $y$ (Leaf) が直接出会った場合**

      - **Action:** $x$ と $y$ を削除し、$(x, y)$ を挿入する。

### 3.3 図解

```text
[Before] LCA = Tensor          [After] LCA = Tensor
      /     \                         |
   (S:Par) (T:Par)                 (New:Par)
   /   \   /   \                   /   |   \
  A    ctx ctx  B              (x, y) ctx  ctx
  |             |
  x             y
```

---

## 4\. Par Merge (Extraction Algorithm)

`{t1, t2}` の導入時に実行される。構造の書き換えは行わず、検証と削除のみを行う。

### 4.1 前提条件

1.  **LCA Type:** LCA は `Par` ノードでなければならない。

    - _Error:_ LCA が `Tensor` の場合 → 「リソースが独立しているため共有できません（Mix 則違反）」。

2.  **Barrier Free:** LCA から `x`, `y` へのパス上に `Tensor` ノードが存在してはならない。

    - _Error:_ パス上に `Tensor` あり → 「変数が Tensor (Tuple) の内部にロックされています」。

### 4.2 アルゴリズム

1.  **削除:**
    `x` と `y` を木から削除する。

2.  **追加**
    LCA に $\{x, y\}$ を追加する。

### 4.3 図解

```text
[Case OK] LCA = Par            [Case Error] LCA = Par
      /       \                       /      \
     x         y                     x     (Tensor) <- Barrier!
                                             /
                                            y
```

---

## 5\. Additive (With / Case) の処理方針

`t1 & t2` や `case` の処理は、Gamma Tree の**スナップショット（Deep Copy)**を用いて行う。

1.  `let snapshot = gamma.clone();`
2.  分岐 A を実行（Gamma を変更）。
3.  `gamma = snapshot;` (ロールバック)
4.  分岐 B を実行（Gamma を変更）。
5.  **整合性検証:** 分岐 A と 分岐 B で、「削除された Leaf の ID 集合」が完全に一致することを確認する。

---

## 6\. 実装上の注意点 (Rust 向け)

- **所有権:** `GammaNode` は `Vec` や `Arena` (SlotMap) で管理し、Tree 構造は ID (`usize`) による参照で表現することを推奨する。これにより、ポインタの書き換え地獄を回避できる。
- **再帰:** `Zip` 操作は再帰的だが、木の深さは高々ソースコードのネスト数に比例するため、スタックオーバーフローの心配は少ない。
- **エラーメッセージ:** エラー発生時、LCA の種類や Barrier となった Tensor の情報をユーザーに提示することで、非常に分かりやすいデバッグ体験を提供できる。
