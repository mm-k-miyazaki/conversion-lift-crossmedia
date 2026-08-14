# クロスメディア IPW-DID パイプライン

## 目的

アプリログ・購買データに格納済みの媒体別プレ／ポストKPIを使い、媒体接触者と非接触者の差をIPW-DIDで推計します。

対象は次の7分析です。

- 晴れ風：Netflix、Tver、Amazon
- メルカリ：Netflix、Tver、Amazon、TV

各媒体で3 KPIを推計するため、合計21個の結果CSVを出力します。今回は媒体別の全体結果だけを対象とし、属性別・FQ別などの内訳分析は行いません。

## 入力

`pipeline.r`と同じWorkspaceフォルダの`data/`配下に次のファイルを置きます。

- `晴れ風_crossmedia_masterdata.csv`
- `メルカリ_crossmedia_masterdata.csv`

分析IDは晴れ風が`モニタID`、メルカリが`mid`です。各ファイルは同一`ID × media`で1行である必要があります。

処置変数は`media_contact_flg`です。

- `1`：接触
- `2`：非接触

## KPI

### 晴れ風

- 購入率：`新プレ_購入フラグ` / `新ポスト_購入フラグ`
- 平均購入回数：`新プレ_購入回数` / `新ポスト_購入回数`
- 平均購入金額：`新プレ_購入金額` / `新ポスト_購入金額`

### メルカリ

- 利用率：`新プレ_利用フラグ` / `新ポスト_利用フラグ`
- 平均利用回数：`新プレ_利用回数` / `新ポスト_利用回数`
- 平均利用時間：`新プレ_利用時間` / `新ポスト_利用時間`

平均KPIは、非購入者・非利用者の0を含む全ID平均です。

## 共変量

すべての媒体で次を使用します。

- 媒体別の新プレ行動カテゴリ
- `SEX`, `AGEID`, `AREA`, `MARRIED`, `CHILD`, `HINCOME`, `PINCOME`
- 対象媒体以外の接触フラグ

新プレ行動カテゴリの定義は次のとおりです。

- 晴れ風：`新プレ_購入回数`が0なら`Non`、0超なら`Any`
- メルカリ：`新プレ_利用時間`が0なら`Non`、0超を媒体別に人数がほぼ均等な`Light / Middle / Heavy`へ3分割

メルカリで同じ利用時間が境界に並ぶ場合は、分析ID順で区分を決めます。旧`プレ_購入回数_segment`と`プレ_利用回数_segment`は使用しません。

他媒体フラグの`1/2/3`は3水準のカテゴリ変数として扱います。

| 案件・対象媒体 | 追加する他媒体フラグ |
|---|---|
| 晴れ風 Netflix | `flg_Amazon`, `flg_Tver` |
| 晴れ風 Tver | `flg_NF`, `flg_Amazon` |
| 晴れ風 Amazon | `flg_NF`, `flg_Tver` |
| メルカリ Netflix | `flg_Amazon`, `flg_Tver`, `flg_TV` |
| メルカリ Tver | `flg_NF`, `flg_Amazon`, `flg_TV` |
| メルカリ Amazon | `flg_NF`, `flg_Tver`, `flg_TV` |
| メルカリ TV | `flg_NF`, `flg_Amazon`, `flg_Tver` |

## Databricksでの実行

1. `pipeline.r`の`PIPELINE_CODE_DIR`を、このコード一式を置いたWorkspaceフォルダへ合わせます。
2. 必要に応じて`00_data_settings.R`の`OUTPUT_ROOT`を変更します。
3. `pipeline.r`を上から実行します。

市場指標に必要なreach・impressions・costが未指定のため、IPA・CPA・ROASの計算は実行しません。

## 検証スクリプト

- `validate_crossmedia.R`：7媒体の行数、接触／非接触件数、ID×媒体一意性、他媒体共変量、新プレ行動カテゴリ設定を読み取り専用で検証します。
- `smoke_crossmedia.R`：Excel成型を除く7媒体×3 KPIのIPW-DIDを実行し、新プレ行動カテゴリ件数、21結果、出力列を検証します。

ローカルRでコードフォルダをカレントディレクトリにした実行例：

```powershell
Rscript validate_crossmedia.R .
Rscript smoke_crossmedia.R . .validation_output
```

## 出力

`OUTPUT_ROOT/run_YYYYMMDD_HHMMSS/`配下に出力します。

- `01_processed/`：媒体で絞り込み、カテゴリを付与した分析用masterdata
- `02_results/`：案件・媒体・KPI別のIPW-DID結果CSV
- `03_excel/`：全CSVをまとめたExcel
- `rds/`：設定、分析用データ、推計結果の中間オブジェクト

結果には案件名、媒体、KPI、`media_contact_flg`、接触区分、N、前期間、後期間、前後差、推計効果、相対リフト、80%信頼区間を含みます。

## 推計上の注意

- WeightItはATTを推計し、DRDIDの`ipwdid`で推計効果と標準誤差を算出します。
- `component_weight`は分析用データに保持しますが、現行仕様どおりWeightIt・DRDIDの推計ウェイトには掛けません。
- 重複ID、ID欠損、`media_contact_flg`の1/2以外、片群欠落、対象媒体フラグとの不整合、非有限IPWウェイトはエラーとして停止します。
