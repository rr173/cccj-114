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
│   └── validate_pipeline.R# 28 项验收断言
├── config/*.json          # 对照策略/窗口/更正/无never 等配置
├── data/snapshots/        # 输入快照（含注入清单 *_manifest.csv）
├── data/corrections/      # 处理时间更正文件
├── output/<version_id>/   # 每个分析版本独立目录
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
