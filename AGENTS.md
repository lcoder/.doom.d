# 仓库规范

## 配置维护

- 本仓库用于 macOS 上的中文 Org 写作。面向用户的说明、进度和回复使用中文。
- `init.el` 选择 Doom 模块并在模块读取配置前加载本机设置。`config.el` 保存 Org、中文编辑与空闲保存设置。`+ui.el` 处理显示与 Org 缩进。`packages.el` 声明额外的软件包，`custom.el` 归 Customize 管理。
- `local.el` 属于本机，已被 Git 忽略。使用现有笔记目录；验证时把临时笔记、Capture、Agenda 和 Org-roam 数据库指向临时目录。缓存、日志与备份放在仓库之外。
- 使用标准 Emacs Lisp 缩进和 lexical binding。个人配置以 `my/` 为前缀。钩子、advice 和定时器需支持重复加载。优先沿用 Doom 现有绑定，新增快捷键须有用户明确要求。
- 更改模块或包后执行 `doom sync`。GNU Emacs 的 Elisp 求值和 ERT 检查通过 `emacsclient`。Neomacs 的字体、颜色和显示问题要在实际 Neomacs 图形窗口中验证。提交前运行 `git diff --check`。
- 本机为私人电脑。提交或推送前核对远程仓库和 GitHub 身份。本仓库属于个人账户 `lcoder/.doom.d`；组织仓库使用对应的公司身份。保留工作区现有改动。提交标题沿用英文 Conventional Commit 格式，正文用简洁中文描述行为变化。

## 本机共享 Emacs 会话

使用 Homebrew 用户服务 `emacs-plus@31` 管理 daemon。日常通过 `/Applications/Emacs Client.app` 或 `emacsclient` 连接默认的 `server`。GUI 与终端共享缓冲区。检查服务使用 `brew services info emacs-plus@31 --json`；重启或停止会结束共享会话，先确认没有待保存的工作和运行中的任务，并取得用户明确授权。

图形客户端启动脚本位于 `~/.local/share/emacs-client/launcher.applescript`，连接已运行的服务。若升级时替换了客户端副本，重新应用本机脚本并签名。Ghostty 与 tmux 的终端描述在 `~/.terminfo`，源码在 `~/.local/share/emacs-client/terminfo/`。
