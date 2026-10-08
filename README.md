# 共享 Doom 配置

适用于 Emacs 29.1 及以上的 macOS。配置沿用 Doom 的包管理、加载生命周期和原有操作。打开文件后自动准备编辑支持，不增加开发菜单或自定义快捷键。

## 模块结构

`init.el` 通过 `doom!` 启用独立的 `:local` 分类，不覆盖 Doom 内置模块。根 `config.el` 和 `+ui.el` 保留中文输入、补全、字体、Org 与个人偏好。

| 模块 | 负责内容 |
| --- | --- |
| `environment` | mise 异步解析、缓冲区环境隔离、缓存与变化通知 |
| `save-format` | 30 秒轻量自动保存、手动保存、EditorConfig、项目格式器与 Apheleia |
| `languages` | 语法库、基础模式回退、语言服务准备与恢复、Rust/Flutter 原生调试适配 |
| `project-commands` | 为 compile/recompile 和 Projectile 提供当前组件目录、环境与明确的项目命令 |

后三个模块只调用 `environment` 的公开接口，彼此不依赖。环境模块不启动语言服务、格式器或项目任务。模块内部使用相对 `load!`；额外包通过所属模块的 `packages.el` 声明。

启动至首页时只注册必要的编辑支持。Org/Org-roam 沿用 Doom 的原生延迟加载；Dart/Flutter、测试和 Dape 适配在相关功能首次加载时准备。Rust 文件仍按原有条件触发调试查询。首次使用会承担一次初始化开销，不新增空闲预热或用户设置步骤。

基础自动保存与 EditorConfig 处理在启动时就绪；项目格式器选择、工具准备和保存适配随 Apheleia 加载，由 Doom 的首次文件生命周期启用。`doom/reload` 会更新已经加载的实现，尚未使用的语言辅助实现继续按需加载。

环境公开接口：

- `+local-env-context`：只读取内存快照，不启动进程。
- `+local-env-ensure`：异步准备并合并重复请求。
- `+local-env-call-with-context`：在快照的目录、环境与工具路径内调用原命令。
- `+local-env-changed-hook`：在受影响缓冲区内通知旧、新快照。
- `+local-env-executable-find`：应用本机工具位置覆盖。

快照状态为 `pending`、`ready`、`unmanaged`、`unavailable` 或 `remote`。消费者自行处理等待、恢复和失败。环境模块关闭时，其他模块退回原生环境；格式器退回 Doom/Apheleia 的原生选择。

## 每台 Mac 的准备

1. 安装 Emacs、Doom 和项目需要的系统工具、SDK、mise 运行时。它们由各台机器独立维护。
2. 同步本仓库后执行本机 `doom sync --env`，再启动 Emacs。环境缓存留在本机 Doom 目录。
3. 打开文件即可。缺失的语法库使用当前 Doom recipe 在本机后台编译；可由 lsp-mode 原生下载器管理的语言服务按需准备，完成后自动接续。

自动准备仅覆盖编辑器语法库和语言服务。缺少编译器、SDK、mise 运行时或业务依赖时保留基础编辑并提示，不执行项目依赖安装、不跳过 mise 信任检查。正常打开和保存不要求先运行 setup 或 refresh。模块 `doctor.el` 提供只读诊断。

`local.el` 已被 Git 忽略，在所有模块默认值声明后、模块配置读取前，通过 `doom-before-modules-config-hook` 加载。已有本机文件和设置会保留。例如：

```elisp
(setq my/dev-notes-directory (expand-file-name "org/" "~")
      my/dev-project-search-directories (list (expand-file-name "projects/" "~"))
      my/dev-font-families '("FiraCode Nerd Font" "SF Mono" "Menlo"))
```

笔记目录必须是已有位置，配置不会建立另一套笔记库。字体仅从当前机器可用字体中选择，由 Doom 初始化所有窗口。特殊工具安装位置可在本机设置 `+local-env-tool-overrides`；原 `my/dev-tool-overrides` 兼容保留。通常应优先刷新本机 Doom 环境缓存。

共享文件不记录用户名、Homebrew 前缀、项目绝对路径、工具版本目录或机器身份。语法库、下载的语言服务、日志和环境缓存留在本机。

## 本机 Homebrew daemon 与客户端

本机使用稳定版 `emacs-plus@31` 源码公式，由公式提供的用户级 Homebrew 服务管理 daemon。安装前确保 Xcode 与 Command Line Tools 满足当前 macOS 的 Homebrew 构建要求；依赖由 Homebrew 安装。

```sh
brew trust d12frosted/emacs-plus
brew tap d12frosted/emacs-plus
brew install emacs-plus@31
doom sync --env --rebuild -U
brew services start emacs-plus@31
```

日常通过 Dock、Spotlight 或 Finder 的 Emacs Client 打开窗口，终端和图形界面共享默认 `server`。关闭窗口保留 daemon 和缓冲区。Git 编辑器等待保存或取消；`:wq` 保存完成编辑，`M-x server-edit-abort` 取消。不要用普通 Emacs 应用作为日常入口。

本机客户端基于公式应用的副本，使用稳定的 `opt` 路径，只连接已有 Server；服务负责启动和恢复。公式默认客户端的按需启动回退已在本机副本中移除。升级后若替换客户端副本，重新应用保存在本机的启动脚本并签名。其他机器须核对自己的客户端行为；具体本机路径和服务约定位于 `AGENTS.md`。

Ghostty 和 tmux 的 terminfo 安装在用户目录，使 daemon 能查找到终端定义和真彩色能力，不依赖终端应用临时注入环境。原生模块和语法库在切换 Emacs 构建后重新检查或构建；环境缓存、客户端应用、终端定义与备份留在仓库外。

使用 `brew services info emacs-plus@31 --json` 检查服务。升级、重启或停止前保存工作，并取得结束共享会话的明确授权。登录启动配置已验证；真实注销再登录、实际键入和视觉体验仍需人工确认。本次未改变全部文件类型的默认应用关联。

## 日常操作

继续使用 Doom 原有 compile/recompile、Projectile 编译/测试/运行，以及语言模块已有的 Flutter 和调试入口。没有 `SPC p m` 开发菜单，不维护第二套任务历史或任务输出管理。

项目命令从当前组件的 package scripts、Cargo、pubspec 或 Just/mise 声明推导。包管理器遵循项目声明和锁文件；没有明确命令时保留原生提示。原生 compilation buffer 负责输出、错误跳转和重跑，重跑沿用该输出缓冲区的目录。文件/目录局部配置和手工设置仍可覆盖默认命令。

Dart 新旧模式共享既有 Flutter 运行、停止、热重载、重启和测试绑定。Flutter 控制命令按最近 pubspec 隔离会话；原生测试也按组件保留进程、输出和环境，并限制测试包的进程设置作用范围。Rust 使用 Dape 的配置和编译生命周期，动态查找已有 LLDB 适配器，从 Cargo 构建输出取得可执行路径；多目标无法确定时使用原生 Dape 配置覆盖。

Rust 的 `SPC m t t` 在 rust-analyzer 就绪时优先使用光标所在测试的原生 Run Test 操作：由语言服务提供完整测试名、Cargo 目标和打印输出参数，结果显示在该测试的原生 compilation 缓冲区。这样只运行所属目标中的精确测试，避免其他目标的零测试汇总；多个匹配目标使用原生选择界面。无匹配单测试、LSP 不可用或自定义了 Rustic runner/Cargo 命令时保留原有行为；`SPC m t a` 继续运行原有的全部测试命令。

`M-o/M-p` 继续执行原有扩选/收缩。缺失或不兼容的原生语法库先回退基础模式；语法库就绪后自动启用对应模式。

`jk` 仅在 Evil 插入（insert）和替换（replace）状态下返回普通状态：先按 `j`，在 0.2 秒内再按 `k`；反向 `kj` 不触发。要输入字面量 `jk`，将两键间隔拉长到超过 0.2 秒。

普通导航、可视选择和操作等待状态保留原有 `j/k` 行为。Treemacs、Dired 及 Doom 默认排除的终端窗口不识别该退出序列；Emacs 输入法与命令输入框沿用现有保护规则。Treemacs 仍使用 `c f` 创建文件、`c d` 创建目录、`?` 查看帮助。

Zen 专注模式按需手动开启，使用 Doom 默认的正文居中、模式栏隐藏和轻微文字放大效果，并通过 `+focus` 淡化当前段落或代码块之外的内容。当前区域保留语法配色；周围文字使用 60% 主题正文色与 40% 背景色混合，随主题切换重新计算。

Rust 使用模式原生的函数或定义边界聚焦，不依赖 LSP 悬停范围；无法识别定义时退回当前行。退出后恢复原来的聚焦范围设置。其他语言和 Org 的范围规则保持原有行为。

- `SPC t z` 或 `M-x +zen/toggle`：切换当前缓冲区的专注模式。
- `SPC t Z` 或 `M-x +zen/toggle-fullscreen`：切换全屏专注模式，进入时收起其他窗口，退出时恢复之前的窗口布局。

使用同一命令再次切换即可退出。首次启用模块后需执行 `doom sync`，保存工作并重启 Emacs 后生效。

## 保存规则

空闲 30 秒只保存修改过的安全文件，跳过远程、只读、间接、加密、锁定和尚未存在的文件；不格式化、不整理空白。

手动保存先写入内容，再异步采用项目规则。文件刚被自动保存过也会尝试格式化。工具准备期间继续编辑会使旧请求失效；Apheleia 继续负责异步取消和内容校验。格式器失败保留已保存内容，无格式变化不重复写盘。

选择顺序为：项目/文件显式覆盖或禁用 → 明确配置或格式化声明 → 项目直接依赖 → 原有语言默认。冲突时普通保存并提示。仅调用已安装工具处理当前文件，不执行全项目格式化脚本。

项目可使用原生 `apheleia-formatter`、`+format-with`、禁用变量与原有跳过格式化操作。旧的 `my/dev-format-choice` 局部变量继续兼容；新增工具适配使用 `+local-save-format-adapters` 与 `+local-save-format-resolvers`。

Rust edition 由后台执行的离线、锁定 Cargo 元数据确定，尊重 workspace 继承和项目 toolchain；Dart 的 stdin 能力也在后台准备。无法可靠确定规则时保留普通保存。

## 维护

本配置仓库不维护测试套件，后续修改不新增或运行测试。

Org 字体只修改当前缓冲区；关闭按键展示时同步停止记录。Org 错误日志位于本机 Doom cache 的 `dev/`，默认每份最多 1 MiB，保留两份历史。

兼容 advice 集中在各所属模块并带能力判断。`+migration.el` 只清理上一版配置拥有的注册，用于安全重载；原先平铺的开发文件、setup 命令、自建任务执行器和菜单均已移除。

| 兼容位置 | 保留原因 |
| --- | --- |
| `+ui.el` | 在 Doom 字体初始化前选择可用字体；不自行初始化 frame。 |
| `save-format/+save.el` | 原生保存 hooks 不覆盖刚被自动保存的未修改文件；在 Apheleia 入口准备工具并继续其原生校验。 |
| `languages/autoload/compat.el` | LSP 缺少统一的异步环境准备入口和跨环境 workspace 筛选接口；Flutter/DAP 需适配组件与异步 provider；Rustic 在 Tree-sitter 模式定位函数时需临时补齐旧语法辅助函数，当前单测试入口优先委托 rust-analyzer 的精确 Run Test 操作。仅在目标函数可用时安装。 |
| `project-commands/+commands.el` | 为原生命令临时绑定组件上下文，输出和历史继续由原生机制管理。 |

生命周期依据 [Doom 官方配置文档](https://github.com/doomemacs/core/blob/master/docs/getting_started.org) 与本机安装的 Doom 实现。
