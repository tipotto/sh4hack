# sh4hack
![sh4hack2](https://user-images.githubusercontent.com/39334151/194248506-345b6404-ff66-4ae4-90bc-fb29cd95221f.png)

## 概要
![metasploit](https://user-images.githubusercontent.com/39334151/194601420-e179b050-6c9c-4dae-829b-fcbe2188e10a.gif)


個人的なハッキング学習を効率化するために開発した、自作ツール（Emacs 拡張機能）です。  
<!--
普段 Emacs というエディタを愛用しており、その特有のキーバインドに慣れ親しんでいるため、
ターゲットサーバーでの作業など、その大部分をローカル PC の Emacs 環境で行うことを目指しています。
-->
主に、

- 頻繁に実行する処理の自動化
- リモートサーバーとローカル PC の相互連携
- Emacs とターミナルの相互連携
- コマンドスニペットの管理

等を行っています。

## 機能の内容
### 1. Metasploit でのセッション確立の自動化
<details>
  <summary>機能の詳細</summary>

  Metasploit での Meterpreter シェルのセッション確立を自動化します。具体的には、以下の処理を実行します。
  - MSFvenom でのペイロードの生成
  - Metasploit の multi/handler の起動
  - ターゲットサーバーへのペイロードの転送、実行 etc.

  想定シナリオ：  
  既存の脆弱性を利用して、ターゲットサーバーへの侵入に成功したが、既存のシェルでは操作性が良くない場合。
</details>

動画：https://youtu.be/RR1Pn-WFjIg

---

### 2. Socat でのセッション確立の自動化
<details>
  <summary>機能の詳細</summary>

  Socat でのセッション確立を自動化します。具体的には、以下の処理を実行します。
  - Socat のダウンロード
  - Socat リスナーの起動
  - ターゲットサーバーへの Socat の転送、実行 etc.
  
  想定シナリオ：  
  既存の脆弱性を利用して、ターゲットサーバーへの侵入に成功したが、既存のシェルでは操作性が良くない場合。  
</details>

動画：https://youtu.be/kQ7sDMRMvIo

---

### 3. Netcat シェルのアップグレードの自動化
<details>
  <summary>機能の詳細</summary>
  
  Netcat シェルのアップグレード処理を自動実行します。一連の処理完了後には、ターゲットサーバー上で以下の操作が可能になります。
  - Ctrl-f, Ctrl-b, Ctrl-a, Ctrl-e でのカーソル移動
  - Ctrl-i でのコマンド補完
  - Ctrl-n, Ctrl-p でのコマンド履歴の操作
  - Ctrl-l でのターミナルのクリア
  - Ctrl-c でのジョブ停止
  - nano, vi 等のエディタでのファイル編集 etc.
  
  想定シナリオ：  
  既存の脆弱性を利用して、ターゲットサーバーへの侵入に成功し、Netcat シェルのセッションが確立済みの場合。    
</details>

動画：https://youtu.be/eMRVoXPYi-U

---

### 4. ターゲットサーバーの検査
<details>
  <summary>機能の詳細</summary>
  
  ターゲットサーバー上に、特定のユーザーのホームディレクトリ、及び .ssh ディレクトリが存在するかを検査します。
  <!-- （SSH に限らず、今後必要に応じて様々な検査項目を追加予定）-->
    
  想定シナリオ：  
  既存の脆弱性を利用して、ターゲットサーバーへの侵入に成功したが、既存のシェルでは操作性が良くないため、公開鍵認証による SSH 接続を検討する場合。    
</details>

動画：https://youtu.be/xaBahJoT4kE

---

### 5. ターゲットサーバーへの SSH 接続
<details>
  <summary>機能の詳細</summary>
  
  ターゲットサーバーに SSH 接続し、ローカル PC の Emacs でリモートファイルを編集できる機能です（Emacs の Tramp という拡張機能を利用）。  
  パスワード認証、公開鍵認証に対応しています。公開鍵認証の場合、以下の処理を自動化します。

  - 認証に用いるキーペアの生成
  - SSH の config ファイルの生成
  - ターゲットサーバーへの公開鍵の転送、登録
  
  想定シナリオ：  
  既存の脆弱性を利用して、ターゲットサーバーへの侵入に成功したが、既存のシェルでは操作性が良くないため、パスワード認証、または公開鍵認証による SSH 接続を検討する場合。
  
</details>

動画：https://youtu.be/BS_PqpEgHG8

---

### 6. リモートファイルの編集
<details>
  <summary>機能の詳細</summary>
  
  ターゲットサーバーのファイルをダウンロードし、ローカル PC の Emacs で編集する機能です。  
  既存のファイルが存在しない場合、新規作成することも可能です。  
  編集内容はターゲットサーバーに自動的に書き込まれます。

  想定シナリオ：  
  Emacs の Tramp を用いた SSH 接続は動作が重いため、より高速な代替機能を検討する場合。
  
</details>

動画：https://youtu.be/J1Z1pWBW4s4

---

### 7. リモートファイルのダウンロード
<details>
  <summary>機能の詳細</summary>
  
  ターゲットサーバー上のファイルを、ローカル PC にダウンロードする機能です。  
  
  想定シナリオ：  
  ターゲットサーバー上のファイルを、ローカル PC の Emacs で操作、閲覧したい場合。
      
</details>

動画：https://youtu.be/KVPHeSrTR7I

---

### 8. ローカルファイルの転送、実行
<details>
  <summary>機能の詳細</summary>
  
  ローカル PC のスクリプトをターゲットサーバーに転送し、実行する機能です。  
  実行結果はローカルのファイルに書き込むこともできるため、Emacs で操作可能です。
  
  想定シナリオ：  
  linpeas.sh や LinEnum.sh など、サーバー情報を列挙するスクリプトをターゲットサーバー上で実行する場合。
    
</details>

動画：https://youtu.be/I3dOF4wjKx4

---

### 9. コマンドスニペットの管理
<details>
  <summary>機能の詳細</summary>
  
  頻繁に実行するコマンドのスニペット（文字列）を管理する機能です。  
  登録したスニペットはワンタッチで表示できるため、手軽に編集、実行することができます。  
  実行するコマンドがワードファイルを必要とする場合、選択したファイルのパスが自動的にスニペットに挿入されます。
  
</details>


動画：https://youtu.be/DAwkUxB7idw

---

### 10. Multi-term の拡張機能の提供
<details>
  <summary>機能の詳細</summary>

  クリップボードの共有など、Multi-term（Emacs 上で動作するターミナルエミュレーター）と Emacs 間の相互連携に必要な機能を提供します。主に以下の操作が可能になります。
  
  - Multi-term でコピーしたテキストを Emacs のクリップボードに保存
  - Multi-term でカット（切り取り）したテキストを Emacs のクリップボードに保存
  - Multi-term で行頭からカットしたテキストを Emacs のクリップボードに保存
  - Multi-term で行末からカットしたテキストを Emacs のクリップボードに保存
  - Multi-term と Emacs で競合するキー（C-x, C-z etc.）における、送信先アプリケーションの切り替え
  - Multi-term のカレントディレクトリのファイルを、Emacs で手軽に検索 etc.
  
</details>