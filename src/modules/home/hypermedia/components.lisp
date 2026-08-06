(defpackage #:home/hypermedia/components
  (:use #:cl)
  (:local-nicknames (#:ah #:almighty-html)
                    (#:h #:almightylisp/hypermedia))
  (:export #:ac-home-layout))

(in-package #:home/hypermedia/components)

(ah:define-component ac-home-layout (&key title children)
  (h:ac-skeleton :title title
    (ah:</>
     (main
       (div :class "simple-landing"
         (section
           (div :class "container"
             (div :class "hero"
               (hgroup :class "hero-header-group"
                 (h1 :class "hero-header" "Almighty Tools For " (em "Almighty") " Programmers")
                 (div :class "logo"
                   (svg :class "logo-svg" :viewbox "0 0 185 209" :fill "none" :xmlns "http://www.w3.org/2000/svg"
                     (path :class "lisp-logo-accent-paths" :d "M14.92 208.412V173.132H26.776V178.46H20.104V187.964H24.904V193.148H20.104V203.084H26.776V208.412H14.92ZM36.8631 208.796C35.4391 208.796 34.2551 208.564 33.3111 208.1C32.3671 207.62 31.6231 206.932 31.0791 206.036C30.5351 205.14 30.1431 204.06 29.9031 202.796C29.6791 201.532 29.5671 200.108 29.5671 198.524L34.6551 198.044C34.6551 198.812 34.6791 199.556 34.7271 200.276C34.7911 200.98 34.8951 201.62 35.0391 202.196C35.1991 202.756 35.4231 203.204 35.7111 203.54C36.0151 203.876 36.3991 204.044 36.8631 204.044C37.6311 204.044 38.1751 203.692 38.4951 202.988C38.8151 202.268 38.9751 201.404 38.9751 200.396C38.9751 199.388 38.7111 198.34 38.1831 197.252C37.6551 196.164 37.0231 195.132 36.2871 194.156C35.0391 192.492 33.9431 190.948 32.9991 189.524C32.0711 188.1 31.3431 186.7 30.8151 185.324C30.3031 183.948 30.0471 182.508 30.0471 181.004C30.0471 179.82 30.1991 178.732 30.5031 177.74C30.8071 176.732 31.2471 175.86 31.8231 175.124C32.4151 174.372 33.1271 173.788 33.9591 173.372C34.7911 172.956 35.7431 172.748 36.8151 172.748C38.1431 172.748 39.2631 172.972 40.1751 173.42C41.1031 173.868 41.8471 174.508 42.4071 175.34C42.9831 176.172 43.3991 177.164 43.6551 178.316C43.9271 179.452 44.0631 180.716 44.0631 182.108L38.9751 182.588C38.9751 181.996 38.9431 181.404 38.8791 180.812C38.8311 180.204 38.7351 179.652 38.5911 179.156C38.4471 178.66 38.2391 178.26 37.9671 177.956C37.6951 177.652 37.3431 177.5 36.9111 177.5C36.2391 177.5 35.7751 177.788 35.5191 178.364C35.2631 178.924 35.1351 179.724 35.1351 180.764C35.1351 182.108 35.4951 183.404 36.2151 184.652C36.9351 185.9 37.9031 187.356 39.1191 189.02C40.5751 190.988 41.7591 192.796 42.6711 194.444C43.5991 196.092 44.0631 197.916 44.0631 199.916C44.0631 201.18 43.9271 202.356 43.6551 203.444C43.3831 204.516 42.9511 205.452 42.3591 206.252C41.7831 207.052 41.0391 207.676 40.1271 208.124C39.2311 208.572 38.1431 208.796 36.8631 208.796ZM53.0819 208.796C51.6579 208.796 50.4739 208.564 49.5299 208.1C48.5859 207.62 47.8419 206.932 47.2979 206.036C46.7539 205.14 46.3619 204.06 46.1219 202.796C45.8979 201.532 45.7859 200.108 45.7859 198.524L50.8739 198.044C50.8739 198.812 50.8979 199.556 50.9459 200.276C51.0099 200.98 51.1139 201.62 51.2579 202.196C51.4179 202.756 51.6419 203.204 51.9299 203.54C52.2339 203.876 52.6179 204.044 53.0819 204.044C53.8499 204.044 54.3939 203.692 54.7139 202.988C55.0339 202.268 55.1939 201.404 55.1939 200.396C55.1939 199.388 54.9299 198.34 54.4019 197.252C53.8739 196.164 53.2419 195.132 52.5059 194.156C51.2579 192.492 50.1619 190.948 49.2179 189.524C48.2899 188.1 47.5619 186.7 47.0339 185.324C46.5219 183.948 46.2659 182.508 46.2659 181.004C46.2659 179.82 46.4179 178.732 46.7219 177.74C47.0259 176.732 47.4659 175.86 48.0419 175.124C48.6339 174.372 49.3459 173.788 50.1779 173.372C51.0099 172.956 51.9619 172.748 53.0339 172.748C54.3619 172.748 55.4819 172.972 56.3939 173.42C57.3219 173.868 58.0659 174.508 58.6259 175.34C59.2019 176.172 59.6179 177.164 59.8739 178.316C60.1459 179.452 60.2819 180.716 60.2819 182.108L55.1939 182.588C55.1939 181.996 55.1619 181.404 55.0979 180.812C55.0499 180.204 54.9539 179.652 54.8099 179.156C54.6659 178.66 54.4579 178.26 54.1859 177.956C53.9139 177.652 53.5619 177.5 53.1299 177.5C52.4579 177.5 51.9939 177.788 51.7379 178.364C51.4819 178.924 51.3539 179.724 51.3539 180.764C51.3539 182.108 51.7139 183.404 52.4339 184.652C53.1539 185.9 54.1219 187.356 55.3379 189.02C56.7939 190.988 57.9779 192.796 58.8899 194.444C59.8179 196.092 60.2819 197.916 60.2819 199.916C60.2819 201.18 60.1459 202.356 59.8739 203.444C59.6019 204.516 59.1699 205.452 58.5779 206.252C58.0019 207.052 57.2579 207.676 56.3459 208.124C55.4499 208.572 54.3619 208.796 53.0819 208.796ZM63.0606 208.412V173.132H74.9166V178.46H68.2446V187.964H73.0446V193.148H68.2446V203.084H74.9166V208.412H63.0606ZM78.7638 208.412V173.132H83.6118L88.4118 191.516L88.8438 193.148H89.1318V173.132H93.8838V208.412H89.5157L84.2358 190.028L83.8038 188.396H83.5158V208.412H78.7638ZM101.157 208.412V178.46H96.2613V173.132H111.237V178.46H106.341V208.412H101.157ZM113.639 208.412V173.132H118.823V208.412H113.639ZM121.606 208.412L126.934 173.132H133.27L138.598 208.412H133.558L132.598 201.212H127.606L126.646 208.412H121.606ZM128.326 195.74H131.878L130.39 183.884L130.246 182.012H129.958L129.814 183.884L128.326 195.74ZM141.342 208.412V173.132H146.526V203.612H153.39V208.412H141.342ZM161.738 208.796C160.314 208.796 159.13 208.564 158.186 208.1C157.242 207.62 156.498 206.932 155.954 206.036C155.41 205.14 155.018 204.06 154.778 202.796C154.554 201.532 154.442 200.108 154.442 198.524L159.53 198.044C159.53 198.812 159.554 199.556 159.602 200.276C159.666 200.98 159.77 201.62 159.914 202.196C160.074 202.756 160.298 203.204 160.586 203.54C160.89 203.876 161.274 204.044 161.738 204.044C162.506 204.044 163.05 203.692 163.37 202.988C163.69 202.268 163.85 201.404 163.85 200.396C163.85 199.388 163.586 198.34 163.058 197.252C162.53 196.164 161.898 195.132 161.162 194.156C159.914 192.492 158.818 190.948 157.874 189.524C156.946 188.1 156.218 186.7 155.69 185.324C155.178 183.948 154.922 182.508 154.922 181.004C154.922 179.82 155.074 178.732 155.378 177.74C155.682 176.732 156.122 175.86 156.698 175.124C157.29 174.372 158.002 173.788 158.834 173.372C159.666 172.956 160.618 172.748 161.69 172.748C163.018 172.748 164.138 172.972 165.05 173.42C165.978 173.868 166.722 174.508 167.282 175.34C167.858 176.172 168.274 177.164 168.53 178.316C168.802 179.452 168.938 180.716 168.938 182.108L163.85 182.588C163.85 181.996 163.818 181.404 163.754 180.812C163.706 180.204 163.61 179.652 163.466 179.156C163.322 178.66 163.114 178.26 162.842 177.956C162.57 177.652 162.218 177.5 161.786 177.5C161.114 177.5 160.65 177.788 160.394 178.364C160.138 178.924 160.01 179.724 160.01 180.764C160.01 182.108 160.37 183.404 161.09 184.652C161.81 185.9 162.778 187.356 163.994 189.02C165.45 190.988 166.634 192.796 167.546 194.444C168.474 196.092 168.938 197.916 168.938 199.916C168.938 201.18 168.802 202.356 168.53 203.444C168.258 204.516 167.826 205.452 167.234 206.252C166.658 207.052 165.914 207.676 165.002 208.124C164.106 208.572 163.018 208.796 161.738 208.796Z" :fill "#FDE047")
                     (path :class "lisp-logo-accent-paths" :d "M14.96 170.412V152.772H17.552V168.012H20.984V170.412H14.96ZM22.0381 170.412V152.772H24.6301V170.412H22.0381ZM29.6816 170.604C28.9696 170.604 28.3776 170.488 27.9056 170.256C27.4336 170.016 27.0616 169.672 26.7896 169.224C26.5176 168.776 26.3216 168.236 26.2016 167.604C26.0896 166.972 26.0336 166.26 26.0336 165.468L28.5776 165.228C28.5776 165.612 28.5896 165.984 28.6136 166.344C28.6456 166.696 28.6976 167.016 28.7696 167.304C28.8496 167.584 28.9616 167.808 29.1056 167.976C29.2576 168.144 29.4496 168.228 29.6816 168.228C30.0656 168.228 30.3376 168.052 30.4976 167.7C30.6576 167.34 30.7376 166.908 30.7376 166.404C30.7376 165.9 30.6056 165.376 30.3416 164.832C30.0776 164.288 29.7616 163.772 29.3936 163.284C28.7696 162.452 28.2216 161.68 27.7496 160.968C27.2856 160.256 26.9216 159.556 26.6576 158.868C26.4016 158.18 26.2736 157.46 26.2736 156.708C26.2736 156.116 26.3496 155.572 26.5016 155.076C26.6536 154.572 26.8736 154.136 27.1616 153.768C27.4576 153.392 27.8136 153.1 28.2296 152.892C28.6456 152.684 29.1216 152.58 29.6576 152.58C30.3216 152.58 30.8816 152.692 31.3376 152.916C31.8016 153.14 32.1736 153.46 32.4536 153.876C32.7416 154.292 32.9496 154.788 33.0776 155.364C33.2136 155.932 33.2816 156.564 33.2816 157.26L30.7376 157.5C30.7376 157.204 30.7216 156.908 30.6896 156.612C30.6656 156.308 30.6176 156.032 30.5456 155.784C30.4736 155.536 30.3696 155.336 30.2336 155.184C30.0976 155.032 29.9216 154.956 29.7056 154.956C29.3696 154.956 29.1376 155.1 29.0096 155.388C28.8816 155.668 28.8176 156.068 28.8176 156.588C28.8176 157.26 28.9976 157.908 29.3576 158.532C29.7176 159.156 30.2016 159.884 30.8096 160.716C31.5376 161.7 32.1296 162.604 32.5856 163.428C33.0496 164.252 33.2816 165.164 33.2816 166.164C33.2816 166.796 33.2136 167.384 33.0776 167.928C32.9416 168.464 32.7256 168.932 32.4296 169.332C32.1416 169.732 31.7696 170.044 31.3136 170.268C30.8656 170.492 30.3216 170.604 29.6816 170.604ZM34.6709 170.412V152.772H37.2629C38.2469 152.772 39.0669 152.988 39.7229 153.42C40.3869 153.852 40.8829 154.464 41.2109 155.256C41.5389 156.048 41.7029 156.98 41.7029 158.052C41.7029 159.124 41.5309 160.048 41.1869 160.824C40.8429 161.6 40.3389 162.208 39.6749 162.648C39.0189 163.08 38.2149 163.316 37.2629 163.356V170.412H34.6709ZM37.2629 160.716C37.7189 160.716 38.0789 160.628 38.3429 160.452C38.6069 160.268 38.7949 159.98 38.9069 159.588C39.0269 159.196 39.0869 158.692 39.0869 158.076C39.0869 157.452 39.0269 156.944 38.9069 156.552C38.7949 156.16 38.6029 155.872 38.3309 155.688C38.0669 155.504 37.7109 155.412 37.2629 155.412V160.716ZM50.0036 170.604C48.9716 170.604 48.1196 170.272 47.4476 169.608C46.7836 168.936 46.4516 167.972 46.4516 166.716C46.4516 165.932 46.5556 165.232 46.7636 164.616C46.9796 163.992 47.2396 163.432 47.5436 162.936C47.8556 162.44 48.1556 161.996 48.4436 161.604C48.0276 160.74 47.7156 159.892 47.5076 159.06C47.2996 158.22 47.1956 157.34 47.1956 156.42C47.1956 155.324 47.4636 154.412 47.9996 153.684C48.5356 152.948 49.3236 152.58 50.3636 152.58C51.3796 152.58 52.1476 152.94 52.6676 153.66C53.1876 154.38 53.4476 155.264 53.4476 156.312C53.4476 157.184 53.2156 158.052 52.7516 158.916C52.2876 159.772 51.7236 160.636 51.0596 161.508C51.4436 162.38 51.7716 163.072 52.0436 163.584C52.3156 164.088 52.5636 164.508 52.7876 164.844C52.9476 164.396 53.0716 163.924 53.1596 163.428C53.2556 162.924 53.3156 162.572 53.3396 162.372L55.4036 162.756C55.3476 163.436 55.2076 164.16 54.9836 164.928C54.7596 165.696 54.4996 166.372 54.2036 166.956C54.3636 167.132 54.5596 167.308 54.7916 167.484C55.0236 167.652 55.2276 167.772 55.4036 167.844V170.604C55.1796 170.604 54.9036 170.508 54.5756 170.316C54.2476 170.124 53.9316 169.9 53.6276 169.644C53.3316 169.388 53.1076 169.164 52.9556 168.972C52.5876 169.46 52.1836 169.856 51.7436 170.16C51.3036 170.456 50.7236 170.604 50.0036 170.604ZM50.1236 168.348C50.4516 168.348 50.7276 168.252 50.9516 168.06C51.1756 167.868 51.3876 167.628 51.5876 167.34C51.2676 166.82 50.9396 166.284 50.6036 165.732C50.2756 165.18 49.9236 164.564 49.5476 163.884C49.3716 164.18 49.2276 164.568 49.1156 165.048C49.0036 165.52 48.9476 165.944 48.9476 166.32C48.9476 166.832 49.0436 167.3 49.2356 167.724C49.4276 168.14 49.7236 168.348 50.1236 168.348ZM50.0996 159.3C50.3876 158.852 50.6556 158.364 50.9036 157.836C51.1516 157.308 51.2756 156.812 51.2756 156.348C51.2756 155.876 51.2116 155.476 51.0836 155.148C50.9556 154.812 50.7156 154.644 50.3636 154.644C50.0196 154.644 49.7796 154.816 49.6436 155.16C49.5076 155.504 49.4396 155.912 49.4396 156.384C49.4396 156.88 49.4996 157.372 49.6196 157.86C49.7476 158.348 49.9076 158.828 50.0996 159.3ZM60.6631 170.412V152.772H66.5911V155.436H63.2551V160.188H65.6551V162.78H63.2551V167.748H66.5911V170.412H60.6631ZM68.5147 170.412V152.772H71.8027L73.2187 161.916L73.3387 162.852H73.3867L73.5067 161.916L74.9227 152.772H78.2107V170.412H75.8107V158.58H75.6667L75.5707 159.324L74.0347 170.412H72.6907L71.1547 159.324L71.0587 158.58H70.9147V170.412H68.5147ZM79.5997 170.412L82.2637 152.772H85.4317L88.0957 170.412H85.5757L85.0957 166.812H82.5997L82.1197 170.412H79.5997ZM82.9597 164.076H84.7357L83.9917 158.148L83.9197 157.212H83.7757L83.7037 158.148L82.9597 164.076ZM92.8038 170.604C92.0678 170.604 91.4398 170.44 90.9198 170.112C90.4078 169.776 90.0158 169.32 89.7438 168.744C89.4798 168.168 89.3478 167.516 89.3478 166.788V156.396C89.3478 155.676 89.4838 155.028 89.7558 154.452C90.0358 153.876 90.4358 153.42 90.9558 153.084C91.4758 152.748 92.0918 152.58 92.8038 152.58C93.3398 152.58 93.8198 152.68 94.2438 152.88C94.6678 153.08 95.0278 153.356 95.3238 153.708C95.6278 154.052 95.8598 154.456 96.0198 154.92C96.1798 155.376 96.2598 155.868 96.2598 156.396V158.82H93.8358V156.396C93.8358 156.028 93.7598 155.72 93.6078 155.472C93.4558 155.224 93.2038 155.1 92.8518 155.1C92.5318 155.1 92.2998 155.228 92.1558 155.484C92.0118 155.74 91.9398 156.044 91.9398 156.396V166.788C91.9398 167.14 92.0158 167.444 92.1678 167.7C92.3278 167.956 92.5558 168.084 92.8518 168.084C93.2118 168.084 93.4638 167.96 93.6078 167.712C93.7598 167.464 93.8358 167.156 93.8358 166.788V164.364H96.2598V166.788C96.2598 167.508 96.1198 168.156 95.8398 168.732C95.5598 169.308 95.1598 169.764 94.6398 170.1C94.1278 170.436 93.5158 170.604 92.8038 170.604ZM101.189 170.604C100.477 170.604 99.8854 170.488 99.4134 170.256C98.9414 170.016 98.5694 169.672 98.2974 169.224C98.0254 168.776 97.8294 168.236 97.7094 167.604C97.5974 166.972 97.5414 166.26 97.5414 165.468L100.085 165.228C100.085 165.612 100.097 165.984 100.121 166.344C100.153 166.696 100.205 167.016 100.277 167.304C100.357 167.584 100.469 167.808 100.613 167.976C100.765 168.144 100.957 168.228 101.189 168.228C101.573 168.228 101.845 168.052 102.005 167.7C102.165 167.34 102.245 166.908 102.245 166.404C102.245 165.9 102.113 165.376 101.849 164.832C101.585 164.288 101.269 163.772 100.901 163.284C100.277 162.452 99.7294 161.68 99.2574 160.968C98.7934 160.256 98.4294 159.556 98.1654 158.868C97.9094 158.18 97.7814 157.46 97.7814 156.708C97.7814 156.116 97.8574 155.572 98.0094 155.076C98.1614 154.572 98.3814 154.136 98.6694 153.768C98.9654 153.392 99.3214 153.1 99.7374 152.892C100.153 152.684 100.629 152.58 101.165 152.58C101.829 152.58 102.389 152.692 102.845 152.916C103.309 153.14 103.681 153.46 103.961 153.876C104.249 154.292 104.457 154.788 104.585 155.364C104.721 155.932 104.789 156.564 104.789 157.26L102.245 157.5C102.245 157.204 102.229 156.908 102.197 156.612C102.173 156.308 102.125 156.032 102.053 155.784C101.981 155.536 101.877 155.336 101.741 155.184C101.605 155.032 101.429 154.956 101.213 154.956C100.877 154.956 100.645 155.1 100.517 155.388C100.389 155.668 100.325 156.068 100.325 156.588C100.325 157.26 100.505 157.908 100.865 158.532C101.225 159.156 101.709 159.884 102.317 160.716C103.045 161.7 103.637 162.604 104.093 163.428C104.557 164.252 104.789 165.164 104.789 166.164C104.789 166.796 104.721 167.384 104.585 167.928C104.449 168.464 104.233 168.932 103.937 169.332C103.649 169.732 103.277 170.044 102.821 170.268C102.373 170.492 101.829 170.604 101.189 170.604Z" :fill "#FDE047")
                     (path :class "lisp-logo-accent-paths" :d "M159 152.412V160.412H117V170.412H109V152.412H159ZM170 170.412H119V162.412H161V152.412H170V170.412Z" :fill "#FDE047")
                     (g :clip-path "url(#clip0_1281_38)"
                       (path :class "lisp-logo-accent-paths" :d "M42.6505 72.9308C43.01 72.8635 43.6305 72.8898 44.0162 72.8763C45.6508 72.8186 51.225 73.1157 49.1217 75.9803C47.3011 78.4598 45.3233 80.9101 43.4148 83.3475L32.0555 97.8768C30.5227 99.8426 28.3101 102.437 27.0062 104.471C21.6566 112.816 39.2013 111.657 43.1275 111.654L58.0747 111.654L84.9585 111.631C90.3662 111.633 95.8194 111.651 101.205 111.622C104.827 111.487 108.749 110.952 111.4 108.239C112.928 106.676 113.219 104.615 111.633 102.988C107.586 99.0378 101.829 100.316 96.597 99.7722C90.0309 99.09 77.42 96.7197 76.7781 88.3604C76.4047 83.4987 82.72 80.1298 86.6887 78.6668C94.2848 75.8669 102.572 75.7534 110.588 75.8888C112.61 75.9152 114.664 75.8188 116.691 75.781C118.908 75.7593 121.125 75.7514 123.342 75.7572C127.112 75.755 132.492 75.3313 135.585 78.0361C136.039 78.4461 136.318 79.0188 136.333 79.6235C136.377 81.4112 133.635 82.3123 132.167 82.4744C129.096 82.8135 125.827 82.6892 122.719 82.6868L106.034 82.683C102.747 82.6482 98.0722 82.8641 95.6027 85.3695C94.5446 86.4436 95.04 88.0183 96.2581 88.7786C99.9385 91.1496 104.503 90.3955 108.596 90.7107C115.757 91.262 123.946 92.8908 129.675 97.3989C135.277 101.808 137.227 109.114 132.469 114.904C126.898 121.684 117.758 124.266 109.356 125.205C107.177 125.45 105.117 125.416 102.948 125.443L92.2702 125.477L64.5565 125.652C54.3956 125.802 44.2338 125.872 34.0718 125.864C24.2182 125.91 12.1402 127.133 3.71989 121.138C-1.67632 117.282 -0.0948632 110.044 3.89003 105.869C12.846 96.4842 22.1358 87.3833 31.4234 78.3321C32.0523 77.7192 32.6252 77.0022 33.2501 76.3785C35.7587 73.875 39.2858 73.2846 42.6505 72.9308Z" :fill "#FEE048")
                       (path :class "lisp-logo-accent-paths" :d "M84.8919 67.6304C89.9997 67.4938 95.7117 67.6192 100.859 67.6189L133.096 67.6257L150.395 67.5997C153.393 67.5907 158.347 67.4929 161.277 67.801C168.099 68.6479 175.749 70.0366 181.249 74.4255C183.46 76.189 185.175 79.4127 182.893 81.7988C179.831 85.0017 174.651 85.6828 170.442 86.0501C167.281 86.3262 162.889 85.6709 160.467 88.1038C159.112 89.4963 161.523 92.8469 162.218 94.2767C163.339 96.5804 164.723 98.8239 165.842 101.14C166.938 103.411 169.732 106.815 167.657 109.179C166.061 110.933 163.058 111.35 160.836 111.554C156.899 111.914 148.55 110.254 146.989 105.979C144.959 100.414 143.209 94.2557 141.269 88.6186C140.722 87.0294 141.152 85.4415 142.329 84.1949C146.857 79.9007 155.556 79.1523 161.334 78.8978C162.051 78.8886 162.769 78.8759 163.486 78.8593C164.561 78.8324 168.698 78.3111 168.363 76.6852C167.827 74.0813 162.832 73.1393 160.573 72.9901C156.376 72.7131 152.067 72.8812 147.841 72.8938L125.007 72.9178L97.934 72.9142L89.3273 72.9122C85.3404 72.9133 77.4561 72.7223 74.1887 75.1834C73.659 75.5814 73.2113 76.0781 72.8702 76.6461C72.0352 78.0239 70.4224 81.6823 69.6634 83.267L63.7349 95.5026C63.0057 97.0193 61.4825 100.466 60.4934 101.558C58.1787 104.113 53.4278 105.488 50.0747 105.696C47.3172 105.837 43.7489 105.886 41.4774 104.08C40.1563 103.03 40.4683 101.415 41.2283 100.155C41.9733 98.9206 42.8087 97.7862 43.5958 96.5802C45.9204 93.0748 48.2633 89.5816 50.6245 86.1006C52.1137 83.8374 53.6206 81.5859 55.1448 79.3462C55.6407 78.6099 57.2009 76.217 57.7688 75.646C63.2495 70.135 73.2896 68.5098 80.7355 67.7595C81.9047 67.6416 83.649 67.7029 84.8919 67.6304Z" :fill "#FEE048"))
                     (path :class "lisp-logo-ink-paths" :d "M14.84 62.4121L20.888 0.672108H29.036L35 62.4121H27.776L26.852 49.8121H22.988L22.064 62.4121H14.84ZM23.534 41.1601H26.39L25.004 17.0101V15.7081H24.836V17.0101L23.534 41.1601ZM37.5003 62.4121V0.672108H44.4723V54.0121H51.6963V62.4121H37.5003ZM55.3011 62.4121V0.672108H63.6591L66.2211 29.0641L66.3471 32.7601H66.5151L66.6411 29.0641L69.2031 0.672108H77.5611V62.4121H70.8831V31.1641H70.7571L70.7151 32.5921L68.3631 62.4121H64.5831L62.1471 32.5921L62.1051 31.1641H61.9791V62.4121H55.3011ZM80.8948 62.4121V0.672108H87.8668V62.4121H80.8948ZM98.0348 63.0841C95.9628 63.0841 94.4088 62.4401 93.3728 61.1521C92.3368 59.8361 91.6508 58.2121 91.3148 56.2801C90.9788 54.3481 90.8108 52.4441 90.8108 50.5681V12.5161C90.8108 10.6681 91.0628 8.77811 91.5668 6.84611C92.0988 4.91411 92.9668 3.29011 94.1708 1.97411C95.3748 0.658108 97.0268 0.000106812 99.1268 0.000106812C100.807 0.000106812 102.179 0.420106 103.243 1.26011C104.335 2.07211 105.189 3.12211 105.805 4.41011C106.421 5.69811 106.841 7.07011 107.065 8.5261C107.317 9.9541 107.443 11.2841 107.443 12.5161V22.8481H100.471V12.1801C100.471 11.4801 100.401 10.7521 100.261 9.99611C100.121 9.21211 99.7428 8.82011 99.1268 8.82011C98.5388 8.82011 98.1748 9.19811 98.0348 9.95411C97.9228 10.7101 97.8668 11.4521 97.8668 12.1801V51.6601C97.8668 52.5561 97.9508 53.3401 98.1188 54.0121C98.3148 54.6841 98.7208 55.0201 99.3368 55.0201C100.009 55.0201 100.443 54.6701 100.639 53.9701C100.863 53.2421 100.975 52.4721 100.975 51.6601V40.1521H99.4628V31.6681H107.443V62.4121H103.831L103.075 60.1441C102.711 61.0961 102.067 61.8241 101.143 62.3281C100.247 62.8321 99.2108 63.0841 98.0348 63.0841ZM110.836 62.4121V0.672108H117.808V26.2081H120.496V0.672108H127.468V62.4121H120.496V35.5321H117.808V62.4121H110.836ZM135.136 62.4121V9.99611H130.012V0.672108H147.232V9.99611H142.108V62.4121H135.136ZM155.599 62.4121V36.1201L148.669 0.672108H156.145L158.665 20.4121L158.875 23.1001H159.085L159.337 20.4121L161.773 0.672108H169.333L162.403 36.1201V62.4121H155.599Z" :fill "white")
                     (defs
                       (clippath :id "clip0_1281_38"
                         (rect :width "184.11" :height "58.695" :fill "white" :transform "translate(0 67.4121)"))))
                   (em "" "Online Book Available Now")))
               (p :class "hero-body"
                 "Common Lisp is the most powerful programming language in history.
 Emacs is the most powerful text editor in history. Now, it is time for you to
 make history by mastering the essentials of these almighty tools with " (em "Almighty
 Lisp: Lisp & Emacs Essentials") ".")
               (a :class "cta" :href "book/essentials" "Learn Lisp & Emacs Immediately"))))
         (section :class "whats-new"
           (div :class "container"
             (h1 :class "special-header" "WHAT'S " (em "NEW"))
             (span :class "special-subheader" "AUGUST 2026 UPDATE")
             (p "The ebook (PDF) version of " (em "Almighty Lisp: Lisp & Emacs Essentials")
               " has been published and is available for purchase for $5.")
             (a :class "cta" :href "https://almightylisp.gumroad.com/l/almighty-lisp-lisp-and-emacs-essentials-ebook" "Purchase Ebook"))
           (div :class "container"
             (span :class "special-subheader" "JULY 2026 UPDATE")
             (p "Big news, almighty bros: " (em "Almighty Lisp: Lisp & Emacs Essentials")
               " has received a major update, the Almighty Lisp Theme Pack 1 is also getting updated, and a second " (em "Almighty Lisp") " theme pack is coming along for the ride."))
           (div :class "container fullwidth"
             (div :class "pricing-card-container"
               (div :class "pricing-card"
                 (hgroup :class "pricing-card-title"
                   (h3 "THE BOOK, UPDATED"))
                 (div :class "pricing-card-body"
                   (p "Fresh since the last release:")
                   (ul
                     (li "A brand-new " (span :class "code" "Windows") " installation guide—Doom Emacs and Common Lisp on native Windows.")
                     (li "An expanded Introduction to ease you onto the almighty path.")
                     (li "Redesigned code and result blocks for cleaner, more readable examples.")
                     (li "New chapter and section headings across every chapter.")
                     (li "Clearer Quicklisp installation instructions for macOS and beyond.")
                     (li "An expanded Errors & Conditions chapter.")
                     (li "A major overhaul of the included accounting and money projects.")
                     (li "Countless fixes throughout."))))
               (div :class "pricing-card"
                 (hgroup :class "pricing-card-title"
                   (h3 "THEME PACK 1, UPDATED"))
                 (div :class "pricing-card-body"
                   (p "Various tweaks have been made to Theme Pack 1. The most significant are:")
                   (ul
                     (li "*-moon variants are now darker and generally warmer for more comfortable night-time coding.")
                     (li "almighty-gear and gear-moon color tweaks.")
                     (li "almighty-macro-moon is noticably more amber.")
                     (li "almighty-titan's bg color is significantly darker for more greater eye comfort."))))
               (div :class "pricing-card"
                 (hgroup :class "pricing-card-title"
                   (h3 "THEME PACK 2, RELEASED"))
                 (div :class "pricing-card-body"
                   (p "Eight more " (em "Almighty Lisp") " themes for Doom Emacs—four light, four dark:")
                   (ul
                     (li "almighty-ena / almighty-ena-moon")
                     (li "almighty-mikasa / almighty-mikasa-moon")
                     (li "almighty-lisp-x / almighty-lisp-x-moon")
                     (li "almighty-saga / almighty-saga-moon"))
                   (p "Try every theme live in the picker below, then read the whole book in your favorite."))))
             (a :class "cta" :href "book/essentials" "Read The Updated Book Immediately")))
         (section
           (div :class "container"
             (h2 "Who This Book Is For")
             (p "If you’re a Lisp-curious veteran programming looking for the perfect book for
getting started with Common Lisp and Emacs, you need to read this book.")
             (p "If you’ve tried to learn Common Lisp in the past, but couldn’t overcome the
Emacs-wall, this book will pull you up and over that wall.")
             (p "If you seek to learn the tools that will take you to another celestial plane of
existence and software engineering ecstasy, first *settle down you insane
person* (but I like that enthusiasm), and then begin reading " (em "Almighty Lisp: Lisp
& Emacs Essentials") " IMMEDIATELY.")))
         (section
           (div :class "container"
             (h2 "Learn The Language & The Editor")
             (p "Learning Common Lisp is easy, but it’s complicated by the fact that Emacs is the
defacto standard editor for it. Other editors support Common Lisp, but none of
them have the rich set of features that you find in Emacs--either for text
editing or for Common Lisp development." (p "Other books on Common Lisp teach the language, but they don’t teach the editor
for the language. Finally, with " (em "Almighty Lisp: Lisp & Emacs Essentials") ", you can
learn the language and the editor with the same book, drastically speeding up
your proficiency with both. You’ll be barfing and slurping while you "
                                           (span :class "code" "C-c") " " (span :class "code" "C-c") " macros in no time."))))
         (section 
           (div :class "container"
             (h2 "What You'll Learn")
             (p "In " (em "Almighty Lisp: Lisp & Emacs Essentials") ", the Common Lisp content covers:")
             (ul
               (li "The Fundamentals (symbols, functions, control flow, etc.)")
               (li "List processing")
               (li "Functional Programming features")
               (li "The most powerful Object-Oriented Programming language features in any language.")
               (li "Macros")
               (li "Errors & Conditions")
               (li "Creating Executables & Deploying")
               (li "... and more."))
             (p "There are several small projects included that you can follow along with to
practice editing with Emacs and become familiar with the essential features
included with Common Lisp. I’ll also introduce you to the ecosystem teach you
proper Common Lisp style.")
             (p "The Emacs content covers:")
             (ul
               (li "Downloading & Installing Doom Emacs")
               (li "Basic Survival Emacs Without Keybindings")
               (li "Text Editing")
               (li "Buffer Navigation & Management")
               (li "Window Navigation & Management")
               (li "Advanced Features Including Project-Level Search & Replace")
               (li "Using the Sly Common Lisp IDE")
               (li "Common Lisp Structured Editing")
               (li "... and more."))
             (p "You won’t need to learn all of Emacs’ features immediately. In fact, you will
begin by using Emacs’ GUI menus for basic operations before advancing to using
keybindings and using more advanced features.")))
         (section
           (div :class "container"
             (h2 "The Almighty Philosophy")
             (p "Common Lisp is a language optimized for ultimate " (i "adaptability") ".
 It’s a generalist’s secret weapon; a programming language that isn’t a master
 at anything, but is quite capable at doing everything.")
             (p "With the rise of LLMs and a rapidly changing software industry, it’s easy to
feel anxious about your own future as a software developer.")
             (p "But you don’t need to worry. You need to " (i "adapt") ". Common Lisp is an " (i "almighty") "
programming language that enables and even " (i "summons") " its users to become almighty.
The macros are waiting. The REPL is loaded. The buffers and windows are at your
command.")
             (p "Be not defeated by the rapidly shifting winds of code and craft. Embrace the
piercing light of destiny, beaming from the flaming horizon over an effervescent
ocean of functions, classes, and parentheses. Become Almighty.")
             (a :class "cta" :href "book/essentials" "Read The Book Immediately")))
         (section 
           (div :class "container"
             (h1 :class "special-header" "ALMIGHTY " (em "THEMES"))
             (p "If you like the color scheme of this website...")
             (p "If reading the code in the " (em "Almighty Lisp: Lisp & Emacs Essentials") " book reminds you of the good ol' days...")
             (p "If you just gotta have more primary colors in your life...")
             (p "Then I have some good news:")
             (p "I am pleased to announce that a second " (em "Almighty Lisp Doom Emacs") " theme pack has arrived alongside the latest book update. That's " (em "sixteen") " themes across two packs—eight light and eight dark—including a theme with the main Almighty Lisp color palette as seen on this website.")))
         (section :class "tall"
           (div :class "container fullwidth"
             (h2 :class "pricing-header" "Pricing")
             (div :class "pricing-card-container"
               (div :class "pricing-card"
                 (hgroup :class "pricing-card-title"
                   (h3 "THEME PACK 1")
                   (p "$10"))
                 (div :class "pricing-card-body"
                   (p "Your purchase supports more Lisp propaganda.")
                   (p "Requires " (a :href "https://github.com/doomemacs/themes?tab=readme-ov-file" "Doom Themes"))
                   (p "Pack includes:")
                   (ul
                     (li "almighty-lisp")
                     (li "almighty-lisp-moon")
                     (li "almighty-titan")
                     (li "almighty-titan-moon")
                     (li "almighty-gear")
                     (li "almighty-gear-moon")
                     (li "almighty-macro")
                     (li "almighty-macro-moon")
                     (li "Free updates of the above themes.")))
                 (a :href "https://almightylisp.gumroad.com/l/almighty-themes-pack-1"
                   (button :class "pricing-card-cta" "PURCHASE THEME PACK 1 IMMEDIATELY")))
               (div :class "pricing-card"
                 (hgroup :class "pricing-card-title"
                   (h3 "THEME PACK 2")
                   (p "$10"))
                 (div :class "pricing-card-body"
                   (p "Your purchase supports more Lisp propaganda.")
                   (p "Requires " (a :href "https://github.com/doomemacs/themes?tab=readme-ov-file" "Doom Themes"))
                   (p "Pack includes:")
                   (ul
                     (li "almighty-ena")
                     (li "almighty-ena-moon")
                     (li "almighty-mikasa")
                     (li "almighty-mikasa-moon")
                     (li "almighty-lisp-x")
                     (li "almighty-lisp-x-moon")
                     (li "almighty-saga")
                     (li "almighty-saga-moon")
                     (li "Free updates of the above themes.")))
                 (a :href "https://almightylisp.gumroad.com/l/almighty-themes-pack-2"
                   (button :class "pricing-card-cta" "PURCHASE THEME PACK 2 IMMEDIATELY")))
               (div :class "pricing-card disabled"
                 (hgroup :class "pricing-card-title"
                   (h3 "THEME PACK 3")
                   (p "$?"))
                 (div :class "pricing-card-body"
                   (p "This pack will be primarily light themes."))
                 (button :class "pricing-card-cta"
                   "UNDER DEVELOPMENT")))))
         (section
           (div :class "container fullwidth"
             (h2 "Theme Packs #1 & #2")
             (div :class "theme-example-container"
               (div
                 (p "Click on any of the buttons below to select a theme and test it out. You can read " (em "Almighty Lisp: Lisp & Emacs Essentials") " using the theme.")
                 (p "I have tried to fine tune the colors to match Emacs as close as possible. Due to
the differences in color rendering in Emacs and different web browsers, however,
the colors on this site may not match exactly on Emacs.")
                 (fieldset :id "theme-select" :class "theme-select" :name "theme-select"
                   :data-script "on change
                                set $theme to the value of <:checked/> in me
                                then set @data-theme of document.documentElement to $theme
                                then set localStorage.theme to $theme
                             end
                             on load
                                set $theme to localStorage.theme
                                if not $theme set $theme to 'almighty-lisp' end
                                set $inputs to <input/> in me
                                if $theme
                                    repeat for input in $inputs
                                        if input.id is $theme 
                                            toggle @checked on input
                                        end
                                    end"
                   (label :class "theme-button"
                     (input :type "radio" :name "theme" :id "almighty-lisp" :value "almighty-lisp") "almighty-lisp")
                   (label :class "theme-button"
                     (input :type "radio" :name "theme" :id "almighty-lisp-moon" :value "almighty-lisp-moon")  "almighty-lisp-moon")
                   (label :class "theme-button"
                     (input :type "radio" :name "theme" :id "almighty-titan" :value "almighty-titan") "almighty-titan")
                   (label :class "theme-button"
                     (input :type "radio" :name "theme" :id "almighty-titan-moon" :value "almighty-titan-moon") "almighty-titan-moon")
                   (label :class "theme-button"
                     (input :type "radio" :name "theme" :id "almighty-gear" :value "almighty-gear") "almighty-gear")
                   (label :class "theme-button"
                     (input :type "radio" :name "theme" :id "almighty-gear-moon" :value "almighty-gear-moon") "almighty-gear-moon")
                   (label :class "theme-button"
                     (input :type "radio" :name "theme" :id "almighty-macro" :value "almighty-macro") "almighty-macro")
                   (label :class "theme-button"
                     (input :type "radio" :name "theme" :id "almighty-macro-moon" :value "almighty-macro-moon") "almighty-macro-moon")
                   (label :class "theme-button"
                     (input :type "radio" :name "theme" :id "almighty-ena" :value "almighty-ena") "almighty-ena")
                   (label :class "theme-button"
                     (input :type "radio" :name "theme" :id "almighty-ena-moon" :value "almighty-ena-moon") "almighty-ena-moon")
                   (label :class "theme-button"
                     (input :type "radio" :name "theme" :id "almighty-mikasa" :value "almighty-mikasa") "almighty-mikasa")
                   (label :class "theme-button"
                     (input :type "radio" :name "theme" :id "almighty-mikasa-moon" :value "almighty-mikasa-moon") "almighty-mikasa-moon")
                   (label :class "theme-button"
                     (input :type "radio" :name "theme" :id "almighty-lisp-x" :value "almighty-lisp-x") "almighty-lisp-x")
                   (label :class "theme-button"
                     (input :type "radio" :name "theme" :id "almighty-lisp-x-moon" :value "almighty-lisp-x-moon") "almighty-lisp-x-moon")
                   (label :class "theme-button"
                     (input :type "radio" :name "theme" :id "almighty-saga" :value "almighty-saga") "almighty-saga")
                   (label :class "theme-button"
                     (input :type "radio" :name "theme" :id "almighty-saga-moon" :value "almighty-saga-moon") "almighty-saga-moon")
                   (label :class "theme-button"
                     (input :type "radio" :name "theme" :id "almighty-mono" :value "almighty-mono") "almighty-mono")
                   (label :class "theme-button"
                     (input :type "radio" :name "theme" :id "almighty-mono-moon" :value "almighty-mono-moon") "almighty-mono-moon")))
               (div
                 (figure :class "almighty-block"
                   (div :class "almighty-block__source-row"
                     (div :class "almighty-block__cell"
                       (pre :class "almighty-block__pre"
                         (code :class "almighty-block__code hl-highlighted lisp"
                           "(defclass flying ()
    ((max-altitude :initarg :max-altitude :initform 1000 :accessor max-altitude)))

(defclass fire-breathing ()
    ((flame-range :initarg :flame-range :initform 30 :accessor flame-range)))

(defclass armored ()
    ((armor-rating :initarg :armor-rating :initform 50 :accessor armor-rating)))

(defclass dragon (flying fire-breathing) ())
(defclass warrior (armored) ())
(defclass mage (flying) ())
(defgeneric attack (attacker target))

(defmethod attack ((attacker fire-breathing) (target armored))
  (format t \"Fire hits armor! ~a% deflected.~%\" (armor-rating target)))

(defmethod attack ((attacker flying) (target flying))
  (format t \"Aerial battle at ~am altitude!~%\" (min (max-altitude attacker) (max-altitude target))))

(defmethod attack ((attacker dragon) (target warrior))
  (format t \"The dragon dives and breathes fire on the warrior!~%\")
  (call-next-method))  ; also runs fire-breathing vs armored

(defparameter *smaug* (make-instance 'dragon :max-altitude 5000 :flame-range 100))
(defparameter *aragorn* (make-instance 'warrior :armor-rating 80))
(defparameter *gandalf* (make-instance 'mage :max-altitude 200))
(attack *smaug* *aragorn*)
"))))
                   (div :class "almighty-block__row"
                     (div :class "almighty-block__cell"
                       (div :class "almighty-block__result-cell"
                         (span :class "almighty-block__result-label"
                           "Returns"
                           (svg :class "almighty-block__cell-label-decoration-svg" :width "5" :height "8" :viewbox "0 0 5 8" :fill "none" :xmlns "http://www.w3.org/2000/svg"
                             (rect :class "almighty-block__cell-label-decoration-rect" :width "2" :height "2")
                             (rect :class "almighty-block__cell-label-decoration-rect" :y "3" :width "2" :height "2")
                             (rect :class "almighty-block__cell-label-decoration-rect" :y "6" :width "2" :height "2")
                             (rect :class "almighty-block__cell-label-decoration-rect" :x "3" :y "3" :width "2" :height "2")
                             (rect :class "almighty-block__cell-label-decoration-rect" :x "3" :y "6" :width "2" :height "2")
                             (rect :class "almighty-block__cell-label-decoration-rect" :x "3" :width "2" :height "2")))
                         (pre :class "almighty-block__result-text"
                           "The dragon dives and breathes fire on the warrior!
Fire hits armor! 80% deflected.
=> NIL"))))))
               (a :class "cta" :href "book/essentials" "Read The Book Using The Selected Theme Immediately"))))
         (section :class "about-author"
           (div :class "container"
             (h2 "About Me, The Author")
             (p "I’m Micah Killian. I’m a former English teacher (ALT) and current freelance
software developer living and working in Saga, Japan. I have a wife and two
daughters. When I’m not rewriting everything in Common Lisp (which you should do
IMMEDIATELY), I’m out with my family climbing mountains, playing at the park,
repairing bicycles, and dreaming of lambdas.")
             (p "You can find me on X at " (a :href "https://x.com/almighty_lisp" "@almighty_lisp") " and you can email me: micah at this website's url."))))))))
