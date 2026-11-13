# 分期上线服务评估：Callaway–Sant’Anna 组别-时间 ATT 面板分析管线

用 R + [`did`](https://bcallaway11.github.io/did/) 包交付的可重复面板数据分析管线。
读取合成地区-月份表，输出**可审查**的组别-时间 ATT、事件研究汇总曲线、权重、
可识别性判定与诊断，**不做仪表盘**。

## 1. 设计要点（与需求逐条对应）

| 需求 | 实现 |
| --- | --- |
| CS 组别-时间效应，不用有偏 TWFE 系数代替 | `did::att_gt` 为目标估计；TWFE 仅在 `twfe_diagnostic.csv` 作偏差对照 |
| never-treated / not-yet-treated 对照可配置 | 配置 `control_group`，内部映射 did 2.1.2 的 `nevertreated`/`notyettreated` |
| 无合法对照返回不可识别及原因，**绝不输出零** | `identifiability.R` 预飞行；不可识别单元格 `att=NA` + `nonid_reason` |
| 预处理检验窗口 | 配置 `pretest_window`（默认 `[-3,-1]`），基于影响函数的联合 Wald 检验 |
| 事件研究聚合权重 | 同时落两套可复算权重：`weight_equal`（等权）与 `weight_group`（组规模份额） |
| 聚类标准误层级 | 配置 `cluster_level`：地区（did bootstrap）/时间（见下“已知限制”）/自定义列 |
| 随机种子并记录入身份 | 配置 `seed`，写入 `identity.json`；同种子逐单元格复现 |
| 窗口/更正/对照变化生成新版本 | 版本哈希含快照 md5、更正文件 md5、规范化配置、窗口、对照、种子等 |
| 缺月不得向前填充 | 仅登记到 `missing_months.csv`，分析面板不产生虚构行 |
| 协变量全缺地区按规则排除并列明 | 唯一预设规则 `exclude`，写入 `excluded_regions.csv` |
| 结果文件分别保留各要素 | 估计值/SE/点态与统一 CI/参与组/对照组/有效样本量/事件时间权重/包版本分文件 |
| 用 did 原始组别结果重算汇总曲线并核对 | `aggregate.R` 双路计算，权重总和=1 核对，等权逐单元格重算自洽性核对 |
| 逐批接入数据（见第 8 节） | `R/ledger/` 接入账本：DBI + RSQLite，修订号判定、删除墓碑、可重放回执、一致快照导出 |

## 2. 目录结构

```
r_did_pipeline/
├── R/
│   ├── utils.R            # 配置、日志、哈希、结果身份
│   ├── validate_data.R    # 快照校验、缺月登记、状态交叉校验、更正、分析面板
│   ├── identifiability.R  # 组别-时点可识别性预飞行（含不可识别原因）
│   ├── estimate.R         # did::att_gt 封装 + TWFE 诊断 + 全 NA 优雅降级
│   ├── aggregate.R        # aggte 事件研究/组/日历聚合 + 显式权重重算
│   ├── diagnostics.R      # 静态图（英文标签，无 CJK 字体依赖）+ 诊断 JSON
│   ├── run_pipeline.R     # 端到端主管线
│   ├── make_synthetic.R   # 合成数据生成器（含真值与数据质量注入）
│   ├── validate_pipeline.R# 28 项验收断言
│   └── ledger/            # 接入账本（本增量，见第 8 节）
│       ├── core.R             # 连接、schema v1、初始化、升级策略、事务封装
│       ├── validate.R         # 批次文件格式与逐行校验（纯函数）
│       ├── ingest.R           # 回执、修订判定、当前指针、过时/重复来件
│       ├── export.R           # 一致快照导出、临时目录发布
│       ├── query.R            # 当前/历史/回执/批次判定/发布查询
│       ├── cli.R              # 命令行公共层与 JSON 错误契约
│       ├── init_ledger.R      # CLI：初始化/校验绑定
│       ├── import_batch.R     # CLI：导入批次
│       ├── export_panel.R     # CLI：导出现行月表与来源
│       ├── query_ledger.R     # CLI：查询账本
│       ├── make_demo_batches.R# 生成可直接运行的演示批次
│       ├── ledger_demo.R      # 场景演示（月报/登记解挂、乱序、删除恢复、更正、冲突、中断）
│       └── concurrency_demo.R # 实际启动多进程 + SIGKILL 演练
├── config/*.json          # 对照策略/窗口/更正/无never 等配置；ledger_export.json 指向导出月表
├── data/snapshots/        # 输入快照（含注入清单 *_manifest.csv）
├── data/corrections/      # 处理时间更正文件
├── data/ledger_demo/      # 演示批次（CSV，可直接导入）
├── output/<version_id>/   # 每个分析版本独立目录
├── tests/ledger/          # 接入账本自动测试（临时库，不依赖 did 拟合）
└── logs/                  # 运行日志
```

## 3. 复现命令

环境为用户态 micromamba（无需 root），R 4.3.3 + did 2.1.2：

```bash
R_BIN=/workspace/.envs/r-did/bin/Rscript

# (1) 生成合成快照（full 含缺月/缺值/协变量全缺/受控冲突；--no-never 无永不处理组）
$R_BIN R/make_synthetic.R data/snapshots/synthetic_full.csv --seed 42
$R_BIN R/make_synthetic.R data/snapshots/synthetic_no_never.csv --seed 42 --no-never

# (2) 运行分析（配置可选 control_group / window / correction / es_weight / cluster_level / seed）
$R_BIN R/run_pipeline.R config/nyt.json          --tag nyt      # not-yet-treated
$R_BIN R/run_pipeline.R config/never.json        --tag never    # never-treated
$R_BIN R/run_pipeline.R config/nyt_corrected.json --tag corrected
$R_BIN R/run_pipeline.R config/nyt_window.json   --tag window   # 窗口/简单权重/时间聚类/新种子

# (3) 端到端验收（28 项断言，退出码 0 即全通过）
$R_BIN R/validate_pipeline.R
```

## 4. 输入表约定

必需列（列名可在配置映射）：`region_id, month(YYYY-MM-01), y, treat,
first_treat_month, x1, x2`。永不处理组的 `first_treat_month` 用配置的
`never_treat_code`（如 `"never"`）单独编码。允许地区缺月。

校验规则（全部留痕，不静默修改数据）：
1. 地区内首次处理月必须恒定；
2. `treat` 必须与首次暴露月一致（`month>=first_treat ⇔ treat=1`），冲突地区整组隔离；
3. 协变量在地区**所有行**完全缺失 → 按 `exclude` 排除并列明；
4. 缺月只登记不填充；行存在但 `y` 缺失单独登记；行级完整观测才入分析面板。

## 5. 输出文件说明（每个版本目录）

| 文件 | 内容 |
| --- | --- |
| `identity.json` | 版本 ID、快照/更正/配置哈希、窗口、对照、权重、预检验窗口、聚类、种子、R/did 版本 |
| `group_time_att.csv` | 每个 (g,t) 的 ATT、SE、`se_method`、点态/统一 CI、可识别标记与原因、处理/对照组、有效样本量 |
| `event_study.csv` | did 聚合曲线 + 手工等权/组份额重算双列、点态与统一 CI |
| `event_study_weights.csv` | 逐 (g,t) 单元格的两套事件时间权重 |
| `weight_sums.csv` | 每个事件期两套权重总和（核对=1） |
| `group_summary.csv` / `calendar_summary.csv` | 组级总效应 / 日历时间总效应 |
| `pretrend_test.csv` | 预处理窗口联合 Wald 检验 |
| `twfe_diagnostic.csv` | 传统 TWFE 系数（仅偏差对照，非目标估计） |
| `identifiability.csv` | 全部目标单元格的可识别性与不可识别原因 |
| `excluded_regions.csv` / `missing_months.csv` / `corrections_log.csv` | 排除、缺月、更正留痕 |
| `analytic_panel.csv` | 实际入模面板（证明缺月未被填充） |
| `fig_*.png` | 事件研究曲线、组别-时间热图、权重总和图（静态，非仪表盘） |
| `diagnostics.json` | 面板/可识别性/预趋势/TWFE 对比/聚合核对汇总 |

### 不可识别原因码
- `no_earlier_period_for_varying_base_period`：首期无 t-1 可做差；
- `group_below_min_size` / `control_pool_below_min_size`：低于最小组规模；
- `no_never_treated_units_available`：never 策略但无永不处理组；
- `not_yet_treated_controls_exhausted`：该期无尚未处理对照；
- `no_later_cohort_for_not_yet_treated_group`：最晚期队列无更晚队列，整组无对照；
- `group_time_cell_not_observed`：该组-时点无观测。

## 6. 已知限制（如实记录，不夸大）

- **时间聚类**：did 2.1.2 拒绝时变 `clustervars`，且仅暴露按单位聚合的影响函数，
  无法在包内重构严格时间聚类方差。`cluster_level="time"` 时保留单位稳健分析 SE，
  并在 `se_method` 列与身份中标注 `did_time_cluster_not_supported_v2.1.2`，
  不静默宣称时间聚类精度。地区级聚类走 did 原生 bootstrap。
- **group 权重口径**：did dynamic 的组权重含 IPW/协变量调整，无法用地区计数精确
  复现。管线以 did 为权威，另给计数近似（`weight_group`）并留痕差异；可严格复算的
  等权口径（`weight_equal`）用于自洽性核对。
- 单元格层不提供统一置信带（did 2.1.2 的统一带在 `aggte` 聚合层给出）。

## 7. 验收覆盖（validate_pipeline.R，28 项）

无处理前效应、已知分期处理效应回收、异质效应下 TWFE≠CS、not-yet-treated 对照
耗尽、缺月不填充、协变量全缺排除、状态冲突隔离、处理时间更正、窗口/权重/聚类/种子
变更产生新版本、无 never 组整体不可识别且无估计、同种子逐单元格复现、改种子点估计
不变而身份变化、权重总和=1、等权重算自洽、CI 完整有限。

## 8. 接入账本：按批导入 -> 导出分析月表

原有管线只能读“已经拼好的整张面板”。本节交付一个可长期使用的本地接入账本：
运营人员通过 R + DBI + RSQLite 逐批导入两类源记录（地区月报、上线登记），
账本负责修订号裁决与当前有效数据维护，再导出既有分析入口可直接读取的地区月表
与逐行来源。**本增量只做落库与拼接，不扩展统计估计。**

### 8.1 环境

账本只依赖 `DBI`、`RSQLite`、`data.table`、`jsonlite`（测试另需 `testthat`），
与现有分析环境的差异仅在于新增 DBI/RSQLite：

```bash
# 已有分析环境（R 4.3.3 + did 2.1.2）之外单独准备账本环境
/workspace/.envs/_bootstrap/bin/micromamba create -y -p /workspace/.envs/r-ledger \
  -c conda-forge r-base=4.3 r-dbi r-rsqlite r-data.table r-jsonlite r-testthat
LEDGER_R=/workspace/.envs/r-ledger/bin/Rscript
```

以下命令均假设在仓库根目录执行。

### 8.2 文件格式（CSV，UTF-8，首行表头，列顺序不限，不允许多余/缺少列）

**月报** `monthly.csv`：

| 列 | 规则 |
| --- | --- |
| `region_id` | 非空字符串（<=512 字节，无控制字符） |
| `month` | 严格 `YYYY-MM`，月份 `01..12`；`2024-13`、`2024-1`、`2024-01-01` 均拒绝 |
| `revision` | 严格十进制正整数（无前导零/符号/小数），`1..2147483647` |
| `op` | `upsert` 或 `delete`（区分大小写） |
| `y`,`x1`,`x2` | `upsert` 时为有限数值；空单元格或字面量 `NA` 表示**显式缺失**；`Inf`/`NaN`/`abc`/`1,5` 拒绝。`delete` 必须为空/`NA` |

**上线登记** `registration.csv`：

| 列 | 规则 |
| --- | --- |
| `region_id` | 非空字符串 |
| `revision` | 同月报 |
| `op` | `upsert` / `delete` |
| `first_treat_month` | `upsert` 时严格 `YYYY-MM` 或字面量 `never`；`delete` 必须为空/`NA` |

- 每个批次至少包含两类文件之一；两类可同批提交，事务上同生共死。
- 一个 SQLite 库只绑定一个 `source_id`（初始化时写入 `meta`，没有改绑命令）。
- `batch_id` 允许 ASCII 字母数字及 `. _ : -`，以字母数字开头，<=128 字符。

`data/ledger_demo/` 下的批次即为可直接运行的样例（见 8.4）。

### 8.3 初始化、导入、查询、导出命令

```bash
LEDGER_R=/workspace/.envs/r-ledger/bin/Rscript
DB=data/ledger/demo.sqlite
OUT=exports

# 1) 初始化（幂等；库已存在且 source_id 一致时返回 exists，不一致拒绝）
$LEDGER_R R/ledger/init_ledger.R --db "$DB" --source-id SRC-EAST

# 2) 导入一个批次（--monthly / --registration 至少给一个；可同时给）
$LEDGER_R R/ledger/import_batch.R --db "$DB" --batch-id D01 \
  --monthly data/ledger_demo/monthly/d01_monthly.csv \
  --registration data/ledger_demo/registration/d01_registration.csv

# 3) 查询
$LEDGER_R R/ledger/query_ledger.R --db "$DB" --what snapshot
$LEDGER_R R/ledger/query_ledger.R --db "$DB" --what current-monthly --region-id R001
$LEDGER_R R/ledger/query_ledger.R --db "$DB" --what history-monthly --region-id R001 --month 2022-06
$LEDGER_R R/ledger/query_ledger.R --db "$DB" --what current-registration
$LEDGER_R R/ledger/query_ledger.R --db "$DB" --what history-registration --region-id R001
$LEDGER_R R/ledger/query_ledger.R --db "$DB" --what receipts   --batch-id D01
$LEDGER_R R/ledger/query_ledger.R --db "$DB" --what outcomes   --batch-id D01
$LEDGER_R R/ledger/query_ledger.R --db "$DB" --what arrivals
$LEDGER_R R/ledger/query_ledger.R --db "$DB" --what releases
$LEDGER_R R/ledger/query_ledger.R --db "$DB" --what check      # 不变量自检

# 4) 导出（每次生成新的 release 目录，不覆盖旧导出）
$LEDGER_R R/ledger/export_panel.R --db "$DB" --out "$OUT"
```

- 所有命令向 stdout 输出**一行 JSON**，日志写 stderr。
- 退出码：`0` 成功；`2` 可预期的校验/业务拒绝（JSON 里带 `error_code`）；
  `1` 内部错误。`--db` 也可用环境变量 `LEDGER_DB` 提供。
- 导入回执中 `"replay": true` 表示这是同批次原文件重传，未重复生效。

### 8.4 可直接运行的样例批次（`data/ledger_demo/`）

由 `Rscript R/ledger/make_demo_batches.R` 生成（幂等，基于
`data/snapshots/synthetic_clean.csv` 中 R001–R003 的真实数值）：

| 批次 | 文件 | 演示内容 |
| --- | --- | --- |
| D01 | `monthly/d01_monthly.csv` + `registration/d01_registration.csv` | R001、R002 各 24 个月；R003 只有月报、尚未登记 |
| D02 | `monthly/d02_monthly.csv` + `registration/d02_registration.csv` | R001 2022-06 修订到 revision 2；R003 补登记（2022-06 起处理） |
| D03 | `monthly/d03_monthly.csv` | 同一批里 R001 2022-07 r3 与 2022-08 r5（乱序 + 更高版本） |
| D04 | `monthly/d04_monthly.csv` | R002 2022-03 撤回（r2, delete） |
| D05 | `monthly/d05_monthly.csv` | 过时来件：R001 2022-07 r2（低于已生效 r3） |
| D06 | `monthly/d06_monthly.csv` | R002 2022-03 r3 upsert（恢复已撤回月份） |
| D07 | `registration/d07_registration.csv` | R003 登记更正为 `never`（r2），跨月重算 treat |
| D08 | `registration/d08_registration_bad.csv` | R002 r1 登记内容冲突，用于制造整批失败 |

一键跑完全部场景并逐条打印 PASS/FAIL：

```bash
$LEDGER_R R/ledger/ledger_demo.R --workdir /tmp/ledger_demo
# 覆盖：月报先到/登记后到解挂、乱序修订、重复报送、删除后旧版本重传、
#       更高版本恢复、登记更正后跨月重算、混合批次冲突整批回滚、
#       一致导出与事务内中断点
```

### 8.5 修订与删除语义

修订号在**各自业务键**内比较：月报键 `(region_id, month)`，登记键 `region_id`；
与文件到达顺序无关。

| 情况 | 结果 |
| --- | --- |
| 来件 revision > 库内该键最高 revision | 写入新版本，推进当前指针 |
| 来件 revision < 最高 revision | 记为 `stale`，完整保留在 `arrival_log`，不改变当前 |
| revision 相同且业务内容一致 | 记为 `duplicate`，不新增版本 |
| revision 相同但内容不同 | `revision_content_conflict`，**整批回滚**，不占 `batch_id` |
| 批内同键同版本内容不同 | `same_revision_conflict`，即使同批还有更高版本也整批失败 |
| `delete` | 作为最高修订号的墓碑保留；此后更低 revision 的 upsert 无法复活 |
| 更高 revision 的 upsert | 恢复该键（回执 outcome=`restore`） |

- 批内判定与行序无关，语义等价于“按键、按 revision 升序逐条到达”：
  同一批里某键的多个递增版本都会进入版本表（历史可查），当前版本取最高者；
  批内与库内之间按同一规则裁决，文件里行的先后不影响结果。
  同批出现两个同版本不同内容时整批失败，即使本批还包含更高版本。
- 成功批次的**版本行 + 当前指针 + 回执 + 逐行判定**在同一个
  `BEGIN IMMEDIATE` 事务中提交；失败或进程被杀则整批不可见，修正后可用同一
  `batch_id` 重试。
- 回执幂等：同 `source_id + batch_id` 原文件重传返回原回执；文件内容变化则
  `batch_replay_conflict`。失败批次不写回执，因此不占用编号。
- 并发：所有导入以 `BEGIN IMMEDIATE` 先取写锁，修订号检查与写入同处一个写事务，
  多进程之间不存在检查-写入窗口；连接设置 `busy_timeout=30000`，WAL 模式。

### 8.6 导出产物（`<out>/<release_id>/`）

| 文件 | 列 | 说明 |
| --- | --- | --- |
| `panel.csv` | `region_id,month,y,x1,x2,first_treat_month,treat` | 地区月表，既有分析入口可直接读取 |
| `missing_registration.csv` | `region_id,month,monthly_record_id,monthly_revision,monthly_batch_id,reason` | 待补登记清单：有有效月报但无有效登记；`reason` 为 `no_registration`（从未登记）或 `registration_withdrawn`（登记已撤回） |
| `missing_registration_summary.csv` | `region_id,n_months,first_month,last_month,months,reason` | 按地区汇总受影响月份与原因 |
| `provenance.csv` | 月表每行对应的 `monthly_record_id/revision/batch_id/received_at` 与 `registration_record_id/revision/batch_id/received_at` | 逐行来源，可回查批次 |
| `manifest.json` | release_id、source_id、创建时间、快照边界、逐文件 md5 与行数 | 发布清单 |

导出规则：

- 只拼接**当前有效**登记；没有登记或登记已撤回的地区**不出现在月表**，其所有
  现存月份逐行进入待补登记清单，**绝不默认 `never`**。
- 登记更正后，下次导出对该地区**所有现存月份**重算：
  `first_treat_month = never` 时 `treat = 0`，否则 `treat = (month >= first_treat_month)`。
- 真实缺月保持缺行（不生成观测），数值缺失保持空单元格。
- 月表、待补清单、来源表在**同一个只读事务**（固定读取快照）内查询；
  写临时目录 `.staging-*`，全部成功且落盘后才原子重命名发布。失败时已有导出
  与已登记 release 均不受影响。
- 既有分析入口只需把 `config/ledger_export.json` 的 `data_path` 指向
  `exports/<release_id>/panel.csv`，其余列名映射已匹配（`R/validate_data.R`
  可直接校验，测试 `test-pipeline-handoff.R` 覆盖该契约，不调用 did 拟合）。

### 8.7 表结构（schema v1）与升级策略

| 表 | 作用 |
| --- | --- |
| `meta(key,value)` | `source_id`、`schema_version`、`created_at` |
| `receipt` | 每批次一行成功回执（`batch_id` 主键、`source_id`、文件内容 md5、摘要 JSON） |
| `monthly_version` | 月报版本行（append-only）；`(region_id,month,revision)` 唯一 |
| `monthly_current` | 每个 `(region_id,month)` 的当前 `record_id` 指针 |
| `registration_version` | 登记版本行（append-only）；`(region_id,revision)` 唯一 |
| `registration_current` | 每个 `region_id` 的当前 `record_id` 指针 |
| `ingest_outcome` | 逐批逐行判定（`insert/replace/restore/delete/duplicate/stale`） |
| `arrival_log` | 过时/重复来件的完整内容（不进版本表，仍可查询） |
| `export_release` / `export_file` | 每次成功导出的清单与逐文件哈希 |

当前有效记录 = 该业务键**最高 revision** 的版本行；由于同版本内容冲突被禁止，
`delete` 一旦取得最高修订号就无法被低版本或同版本覆盖，只能被更高版本 upsert 恢复。
`monthly_current`/`registration_current` 只是查询加速指针，`query_ledger.R --what check`
会校验它与版本表一致。

**升级策略**：`LEDGER_SCHEMA_VERSION` 当前为 `1`。未来变更在
`R/ledger/core.R` 的 `SCHEMA_MIGRATIONS` 中登记 `from -> to` 迁移函数；
`init_ledger.R` 与导入/导出入口都会先检查版本：
库版本高于代码则拒绝（`schema_newer_than_code`），低于代码则在
`BEGIN IMMEDIATE` 内执行迁移并回滚失败，保证库要么完整升级要么保持旧版本。

### 8.8 自动测试与多进程演练

```bash
LEDGER_R=/workspace/.envs/r-ledger/bin/Rscript

# 全部账本测试（临时库；353 项断言；不依赖 did 拟合）
$LEDGER_R tests/ledger/run_tests.R
$LEDGER_R tests/ledger/run_tests.R concurrency   # 只跑多进程部分

# 场景演示（月报/登记解挂、乱序修订、重复报送、删除与恢复、更正重算、混合冲突、中断点）
$LEDGER_R R/ledger/ledger_demo.R --workdir /tmp/ledger_demo

# 真实多进程演练：两个并发写进程 + 同一批次竞争 + 事务内 SIGKILL 后重启重传
$LEDGER_R R/ledger/concurrency_demo.R --workdir /tmp/ledger_cc --pause 60
```

测试文件与覆盖：

| 文件 | 覆盖 |
| --- | --- |
| `test-format.R` | 月份/修订号/数值/`never` 校验、表头契约、delete 载荷、批内冲突与折叠 |
| `test-ingest.R` | source_id 绑定、修订高低、同版本重复/冲突、删除墓碑与恢复、批内乱序、回执重放、失败不占号、中断点、同批两类记录 |
| `test-revision-semantics.R` | 随机到达顺序下与参考实现一致；删除后低版本永不复活；顺序无关性 |
| `test-export.R` | 待补登记、登记撤回、treat 重算、缺月/缺值保留、逐行来源、发布不覆盖、失败保留旧导出、一致快照 |
| `test-concurrency.R` | **实际启动两个写入进程**并发导入不同批次；同批次两进程一提交一重放；**SIGKILL 停在提交前的导入进程**后核对回执/当前数据/历史记录一致，重启重传只生效一次；CLI 退出码 |
| `test-cli.R` | README 中的 CLI 命令可直接运行、幂等初始化、错误 JSON |
| `test-pipeline-handoff.R` | 导出 `panel.csv` 通过既有 `validate_data.R` 校验；缺月/缺值不被填充 |

> 账本测试与 `validate_pipeline.R` 相互独立：前者只验证接入、拼接与导出契约，
> 不调用 `did::att_gt`，因此在没有 did 的环境中也能全部通过。
