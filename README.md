# 用 Doom Emacs 写 Org 笔记

这是一套用于 macOS 的中文 Org 写作配置。笔记、日程和快速记录共用 `org-directory`，Org-roam 的笔记放在其 `roam/` 子目录。文档可导出为 HTML、Markdown、纯文本、iCalendar、LaTeX 和 ODT。

## 打开笔记

| 操作 | 按键 |
| --- | --- |
| 在笔记目录查找文件 | `SPC n f` |
| 用 Dired 浏览笔记目录 | `SPC n F` |
| 打开 Agenda | `SPC n a` |
| 用 Capture 记录事项 | `SPC n n` |
| 查找 Org-roam 节点 / 插入节点链接 | `SPC n r f` / `SPC n r i` |
| 查看当前节点的反向链接 | `SPC n r r` |
| 在 Org 文档中打开导出菜单 | `C-c C-e` |
| 切换专注写作 / 全屏专注写作 | `SPC t z` / `SPC t Z` |

## 编辑与保存

- 用 `C-\` 切换 pyim 全拼输入法，候选词显示在底部。Org 正文支持中文标点，中英文间距由自己控制。
- 在 Evil 中用 `/` 或 `?` 向前或向后搜索。输入 `shiyong` 或 `sy` 都能找「使用」，也能匹配英文原文。超过八个字母的查询按原样搜索。文件名补全支持打乱顺序输入多个关键词。
- 用 `jk` 退出插入或替换状态。普通和可视状态下，`j`、`k` 按屏幕上的可见行移动。用 `M-o` 扩大选区，`M-p` 缩小选区。中文括号和引号可以通过 Evil Surround 操作。
- 新建 Org 文档可使用 Doom 文件模板。中文表格由 Valign 对齐，字体优先使用 Maple Mono NF CN。
- 在 Neomacs 图形窗口中，按 `⌘=` 或 `⌘+` 放大全局字号，`⌘-` 缩小，`⌘0` 恢复原字号。Org 标题和文档里的 Rust 等源码块使用当前主题的颜色。
- 用 `C-x C-s` 或 `:w` 手动保存。Emacs 空闲 30 秒后会写回已经落盘、有修改且可写的本地 Org 文档。新文件须先手动保存一次。只读、加密、远程、被其他进程锁定或在磁盘上已被外部修改的文件会跳过。

## 在另一台 Mac 上使用

将配置放在 Doom 用户目录。若要指定笔记位置，在该目录中创建 Git 已忽略的 `local.el`，指向已有的笔记库。本机使用 iCloud Drive 下的 `org/`。也可以在这里调整字体。

```elisp
(setq my/dev-notes-directory
      (expand-file-name "Library/Mobile Documents/com~apple~CloudDocs/org/" "~")
      my/dev-font-families '("Maple Mono NF CN" "SF Mono" "Menlo"))
```

更改模块或软件包后，运行 `doom sync` 并在保存工作后重启 Emacs。本机日常写作使用 Neomacs 图形客户端。通过 Emacs Client 打开的是 Homebrew 管理的另一套共享会话。使用 `brew services info emacs-plus@31 --json` 查看服务状态。
