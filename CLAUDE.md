# lisp-dialect-bridge — Claude Code 作業ルール

## このリポジトリの位置づけ

- *NeLisp 拡張 package* として位置づけ。anvil-pkg と並列の独立 repo。
- 戦略上の役割: Guix recipe import / Lisp ecosystem 取り込み / polyglot Lisp Machine の translation layer。詳細は anvil-pkg/docs/design/03-strategy-vision.org 参照。
- runtime は最終的に NeLisp に依存予定だが、Phase 1 では Elisp で書き、 NeLisp 上でも Emacs 上でも動くように保つ。

## 設計 invariant

1. **DSL の説明は "Emacs Lisp"、NeLisp は runtime 詳細** — anvil-pkg と同方針 (anvil-pkg memory: feedback_nelisp_runtime_vs_elisp_language)。
2. **IR は dialect-neutral** — Scheme/CL 固有 semantics は `:meta` フィールドにのみ残し、IR の core 形は Lisp 共通最小集合。
3. **Macro hygiene 戦略を変えるときは 01-overview.org の "Macro hygiene strategy" 節を更新してから code を変える** — 後戻り cost が大きい設計判断のため。
4. **subset を絞る、reject は loudly** — 翻訳できない form は silent failure ではなく明示的 error。エラー型は `ldb-unsupported-form-error` 等を将来定義。
5. **anvil-pkg と疎結合** — bridge は anvil-pkg を require しない。Phase 1 importer の出力は文字列または S-式で、anvil-pkg 側で読み込む。

## コーディング規約

- Elisp: `lexical-binding: t` 必須、autoload cookie を public API に付ける
- GPL-3.0-or-later (anvil-pkg / NeLisp と整合)
- ERT テスト: `test/ldb-*-test.el` (Phase 1 から)
- design doc: `docs/design/NN-<topic>.org`

## 命名規約 (3-layer、anvil-pkg と平行)

| Layer | Convention | Example |
|-------|-----------|---------|
| project / repo / brand | `lisp-dialect-bridge` | repo 名 |
| 公開 Elisp API | `ldb-` prefix (短形) | `ldb-guix-import-file`, `ldb-translate` |
| 内部実装 | `ldb--` (private double-dash) | `ldb--ir-node`, `ldb--emit-form` |
| MCP tool | (Phase 2+ で検討) | n/a Phase 1 |

`ldb-` は "lisp-dialect-bridge" の短縮形。CL `cl-` / org `org-` と同種の prefix claim。衝突確認済 (Linux distro database `ldb` あるが Emacs ecosystem では未使用)。

## Phase 0 → Phase 1 移行条件

- 01-overview.org に IR / hygiene 戦略 / Phase 1 contract が locked ✓ (Phase 0 終了条件)
- Phase 1 着手時に作るもの:
  - `lisp-dialect-bridge.el` 最小 loader (`provide` のみ)
  - `ldb-ir.el` IR node 定義 + helpers
  - `ldb-scheme.el` Scheme reader (用途: 文字列 → S-式 + 型情報)
  - `ldb-emit-elisp.el` IR → Elisp source
  - `ldb-guix-importer.el` Phase 1 entry — `(ldb-guix-import-file FILE SYM)`
  - ERT 8-10: parse / IR shape / emit golden / round-trip / unsupported reject

## 参考になる anvil-pkg 構造

- `anvil-pkg-dsl.el` — Guix-style declarative record の Elisp DSL 例。bridge の出力 target としても直接使う
- `anvil-pkg/docs/design/02-dsl.org` — IR + renderer 分離パターンの先例
- `anvil-pkg/docs/design/03-strategy-vision.org` — 戦略 frame の親 doc

## 依存関係 (planned)

- Emacs 29+
- (Phase 2+) Guile or Chicken Scheme — pre-expand step に外部 Scheme インタプリタを使う案 (`docs/design/01-overview.org` の hygiene strategy 参照)
- (Phase 4+) Common Lisp implementation (SBCL) — 同様
