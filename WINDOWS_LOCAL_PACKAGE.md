# Windows 本地版软件封装说明

这个项目已经按“解压即用”的 Windows 本地版方式准备了启动和打包脚本。

## 直接启动

双击项目根目录下的 `启动软件.bat`，或运行英文版 `start_app.bat`。

启动脚本会使用项目内置的 `R\bin\Rscript.exe`，并在浏览器打开：

```text
http://127.0.0.1:3838
```

关闭启动窗口即可停止软件。

## 封装前准备本地 R 包

项目内置 R 必须包含 Shiny 和各分析模块需要的第三方包。运行：

```powershell
.\R\bin\Rscript.exe .\scripts\prepare_portable_packages.R
```

这个脚本会把当前电脑已经安装的依赖包复制到：

```text
R\library
```

如果提示缺少包，需要先在当前 R 环境安装缺失包，再重新运行。

## 检查依赖

```powershell
.\R\bin\Rscript.exe .\scripts\check_dependencies.R
```

看到 `DEPENDENCY_CHECK_OK` 就说明本地 R 包齐全。

## 生成发布包

```powershell
powershell -ExecutionPolicy Bypass -File .\scripts\package_windows.ps1
```

生成结果在：

```text
dist\AgingGeneMLApp-Windows
dist\AgingGeneMLApp-Windows.zip
```

把 zip 发给别人，对方解压后双击 `启动软件.bat` 即可使用。

## 生成 exe 安装包

电脑已安装 Inno Setup 6 时，可以运行：

```powershell
powershell -ExecutionPolicy Bypass -File .\scripts\build_installer.ps1
```

生成结果在：

```text
dist\installer\AgingGeneMLApp-Setup.exe
```

安装后会创建开始菜单快捷方式；安装末尾也可以直接启动软件。

## 可选：指定端口

如果 3838 被占用，可以这样启动：

```bat
set APP_PORT=3839
启动软件.bat
```
