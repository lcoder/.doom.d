# 仓库规范

## 项目结构与模块组织

本仓库存放适用于 macOS 的共享 Doom Emacs 配置，面向 Emacs 29.1 及以上版本。`init.el` 用于启用 Doom 模块；`packages.el` 用于声明软件包及其固定版本；`config.el` 和 `+ui.el` 用于保存个人偏好。四个私有 Doom 模块位于 `modules/local/` 下：`environment`、`save-format`、`languages` 和 `project-commands`。本机覆盖配置通过 `doom-before-modules-config-hook` 加载。`README.md` 记录安装配置方法和日常命令。`custom.el` 保存 Customize 设置。运行时资源和缓存应放在本仓库之外。

## 开发命令

在本目录中使用已安装的 Doom 可执行文件运行以下命令：

- `doom sync`：同步 `init.el` 或 `packages.el` 的变更；完成后重启 Emacs。
- `doom sync --env`：同步软件包，并在支持此功能的 Doom 版本中刷新本机环境缓存。
- `M-x doom/reload`：重新加载配置变更。
- 各模块的 `doctor.el` 文件提供只读的能力检查。打开文件时会自动准备相应的编辑器支持，无需运行额外的设置命令或使用自定义开发菜单。
- `git diff --check`：提交前检查空白字符问题。

## 代码风格与命名约定

使用空格和标准 Emacs Lisp 缩进（函数体缩进两个空格），并通过 `indent-region` 调整缩进。保留模块文件头部的 lexical-binding 声明。名称采用 kebab-case（小写单词以连字符分隔）；模块功能使用 `+local-<module>-` 前缀，个人偏好使用 `my/` 前缀，内部辅助函数使用 `--` 分隔。为函数和可配置变量添加文档字符串。使用 `after!` 和 `use-package!` 配置软件包。确保钩子、函数增强（advice）、定时器和按键绑定可以安全地重新加载，不会重复注册。

所有必要的 Elisp 求值都使用 `emacsclient`；保持用户现有的图形界面和后台共享会话运行。

## 本机 Emacs daemon 与 Git 编辑器

2026-10-08 本机已迁移至 Homebrew Emacs Plus 31.1，使用 macOS 27 对应的 Xcode / Command Line Tools 27.0。MacPorts 的 Emacs 31.1 和未激活的 30.2 均已通过包管理器卸载；其他 ports 和共享依赖保留。其他机器使用前应核对安装路径与服务状态。

- 使用用户级 Homebrew 服务 `emacs-plus@31`，登录时运行 `emacs --fg-daemon`；服务已启用 `RunAtLoad` 和 `KeepAlive`。daemon 的启动与恢复统一由此服务管理，不另建重复的启动服务。
- 日常图形入口为 `/Applications/Emacs Client.app`，Dock 已替换为该客户端。终端和图形客户端连接默认名为 `server` 的同一个 Server，共享缓冲区、配置和运行中的任务。不要将普通 `Emacs.app` 作为日常启动入口。
- 本机客户端使用 `/opt/homebrew/opt/emacs-plus@31/bin/emacsclient -s server -c -n` 创建图形窗口，再通过 Elisp 聚焦窗口；不使用 `open -a Emacs` 或 `-a ""`。连接失败时提示检查服务，避免启动第二个 Emacs。支持 Finder 文件打开、拖放和带空格的路径。
- 启动器源码位于 `~/.local/share/emacs-client/launcher.applescript`。客户端是公式应用的本机副本；升级后若替换副本，须重新编译该严格连接脚本并重新签名，不能直接保留公式默认的按需 daemon 回退行为。客户端使用稳定的 `opt` 路径。
- Git 全局 `core.editor` 为 `/opt/homebrew/bin/emacsclient -t`。Git 编辑器不使用 `-n`，须等待编辑完成；也不使用 `-a ""`。Doom/Evil 中使用 `:wq` 保存并完成编辑，取消使用 `M-x server-edit-abort`。不要因配置了编辑器而自动执行实际项目的提交或 amend。
- Ghostty 与 tmux 的终端定义已安装到用户级 `~/.terminfo`，源码分别位于 `~/.local/share/emacs-client/terminfo/xterm-ghostty.src` 和 `tmux-256color.src`。两者补充 Emacs 使用的 `setf24` / `setb24` 能力；修改后使用 `/usr/bin/tic -x -o ~/.terminfo <源码文件>` 编译。只设置 Ghostty 应用的 `TERMINFO` 不足以供常驻 daemon 查找终端定义。处理配色问题时先检查颜色能力，不通过硬编码 face 颜色掩盖降级问题。
- 关闭客户端窗口不会结束 daemon；重新打开客户端可继续使用同一会话。基础编辑工具优先使用 Homebrew，Node 沿用 mise，Rust 沿用 rustup。Flutter SDK 位于 Homebrew，独立 Dart 公式保留但解除链接，使用 Flutter 自带 Dart。
- 使用 `brew services info emacs-plus@31 --json` 查看服务状态。启动使用 `brew services start emacs-plus@31`，重启使用 `brew services restart emacs-plus@31`，停止使用 `brew services stop emacs-plus@31`；重启或停止会结束整个共享会话，必须先确认保存状态，并取得用户对结束会话的明确授权。

本次已验证原生编译、vterm、TS/TSX、Rust、Dart、JSON/YAML 和 HTML/CSS 语言服务，以及手动保存格式化与空闲保存不格式化。图形窗口关闭后缓冲区和 daemon PID 保留；带空格路径可通过客户端打开。真实 Ghostty 中的终端客户端识别到 16777216 色，背景为 `#212337`，高亮行为 `#2f334d`；在临时仓库通过 Emacs 原生命令接口执行 `:wq` 对应命令和取消编辑，验证 amend 完成、取消后提交不变。这不等于实际键入或视觉验收。

简单 PTY 协议仿真的 Git 编辑器验收曾触发 Emacs 31.1 的 `tty_write_glyphs_1` 崩溃，服务已恢复；不要把该仿真器的结果作为真实终端的验收结论。真实 Ghostty 的原生命令接口验收期间 daemon PID 保持不变。没有本次启动耗时基准。登录启动配置已检查，实际退出并重新登录后的启动行为尚未实测。验收记录、旧应用和配置备份位于 `~/.local/state/emacs-daemon-migration/`，不放入仓库。

## 提交与拉取请求规范

遵循近期提交历史：提交标题使用英文 Conventional Commit 格式，例如 `fix(doom): guard package lookup`；正文使用简洁的中文描述行为变化。每次提交应聚焦于明确的变更。拉取请求应说明变更内容、相关版本，在适用时关联问题，并为界面变化提供截图。

## 配置与代理要求

面向用户的说明、进度更新和最终回复统一使用中文。

本机路径和覆盖配置保存在已被 Git 忽略的 `local.el` 中；绝不提交敏感信息、环境缓存或生成的二进制文件。根据所需能力查找工具，并遵守项目配置。保留工作区中与当前任务无关的变更。提交或推送前，核对远程仓库，并区分个人与公司的 GitHub 身份；当前检出使用个人仓库 `lcoder/.doom.d`。

## 模块边界

遵循已批准的 Doom 原生架构。不要添加自定义开发快捷入口、设置流程或任务运行器。调用方只能使用 `environment` 模块的公开 API 和常规钩子，不得访问其内部缓存。在输入或保存的执行路径上，不得同步调用命令行工具进行探测。只允许自动准备语法库和受支持的语言服务器；SDK、mise 运行时和业务依赖应保留各自的常规安装流程。在运行时检查可选模块是否可用，并确保重新加载具有幂等性。

## 关于快捷键
除非用户明确要求，否则不要新增自定义快捷键。优先复用已有绑定或通过 `M-x` 调用命令，不要为新增功能自动配置快捷键。
新增绑定应遵循 Doom Emacs 现有的按键分组和作用域约定，优先使用 `map!`；添加前检查已有绑定，避免重复或意外覆盖。
