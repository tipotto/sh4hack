# sh4hack
![sh4hack2](https://user-images.githubusercontent.com/39334151/194248506-345b6404-ff66-4ae4-90bc-fb29cd95221f.png)

## Description

![metasploit](https://user-images.githubusercontent.com/39334151/194247080-fd3069b0-ab74-4be4-b047-690e9495ad7f.gif)

個人的なハッキング学習を効率化するために開発した、自作ツール（Emacs 拡張機能）です。  
なるべく全ての作業をローカル PC の Emacs 環境で行うことを目指しています。


## Overview
### 1. Metasploit でのコネクション接続の自動化
<details>
  <summary>主に以下の処理を自動化します。</summary>
    
  - MSFvenom でのペイロードの生成
  - Metasploit の multi/handler の起動
  - ターゲットサーバーへのスクリプトの転送、実行 etc.
</details>

動画はこちらになります。  
YouTube: https://youtu.be/FLfxcJAL4kM

---

### 2. Socat を用いたコネクション接続の自動化
<details>
  <summary>主に以下の処理を自動化します。</summary>
  
  - Socat のダウンロード
  - Socat リスナーの起動
  - ターゲットサーバーへの Socat バイナリの転送、実行 etc.
</details>

動画はこちらになります。  
YouTube: https://youtu.be/1PXs0Py_ETQ

---

### 3. Netcat シェルのアップグレードの自動化
<details>
  <summary>以下の操作を可能にします。</summary>
  
  - Emacs キーバインドでのカーソル移動、文字の削除
  - Tab キーによるコマンド補完
  - コマンド履歴の操作
  - nano, vi 等のエディタでのファイル編集
  - Ctrl-C によるジョブ停止 etc.
</details>

動画はこちらになります。  
YouTube: 

---

### 4. ターゲットサーバーの検査
ターゲットサーバー上に、特定のユーザーのホームディレクトリや .ssh ディレクトリが存在するかを検査する機能です。

動画はこちらになります。  
YouTube: https://youtu.be/EmOyeAAvuRw

---

### 5. リモートサーバーへの SSH 接続
ローカル PC の Emacs でリモートサーバーのファイルを編集できる機能です (Emacs の Tramp という拡張機能を利用)。  
パスワード認証、公開鍵認証に対応しています。

<details>
  <summary>公開鍵認証では以下の処理を自動化します。</summary>

  - 認証に用いるキーペアの生成
  - SSH の config ファイルの生成
  - リモートサーバーへの公開鍵の転送、登録  
</details>

動画はこちらになります。  
YouTube: https://youtu.be/BS_PqpEgHG8

---

### 6. リモートファイルの編集
リモートサーバーのファイルをダウンロードし、ローカル PC の Emacs で編集する機能です。  
また既存のファイルが存在しない場合、新規作成することも可能です。  
編集した内容はリモートサーバーに自動的に書き込まれます (Emacs の Tramp は動作が重いため、代替機能として実装)。

動画はこちらになります。  
YouTube: https://youtu.be/77VgX-9GCrE

---

### 7. ローカルファイルの転送、実行
スキャニング用のスクリプトなどをリモートサーバーに転送、実行する機能です。  
実行結果はローカル PC のファイルに書き込まれるため、Emacs で操作可能です。

動画はこちらになります。  
YouTube: 

---

### 8. リモートファイルのダウンロード
リモートサーバー上のファイルをローカル PC にダウンロードする機能です。

動画はこちらになります。  
YouTube: 

---

### 9. コマンドスニペットの管理
頻繁に実行するコマンドのスニペットを登録できる機能です。  
手軽にスニペットを編集したり、ワンタッチでコマンドを実行することができます。

動画はこちらになります。  
YouTube: 

---

### 10. Multi-term の拡張機能の提供
Multi-term（Emacs 上で動作するターミナルエミュレーター）ではサポートされていない、Multi-term と Emacs 間のクリップボード共有など、相互連携に必要な機能を提供します。
