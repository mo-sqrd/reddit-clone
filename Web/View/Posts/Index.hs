module Web.View.Posts.Index where
import Web.View.Prelude

data IndexView = IndexView { posts :: [Post] }


instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h2>Hello {currentUser.username}!</h2> 
        <a class="js-delete js-delete-no-confirm" href={DeleteSessionAction}>Logout</a>

        <h1>Post Board<a href={pathTo NewPostAction} class="btn btn-primary ms-4">+ New</a></h1>


        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th> Recent Posts</th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach posts renderPost}</tbody>
            </table>
            
        </div>

        -- JS: react POST + DOM update (runs on pages that include this view)
        <script>{raw|
            (function(){
              function updateSummary(postId, counts){
                // update inline emoji buttons with counts
                const order = ['❤️','👍','👎','😂','😍','😢','😱','😡']
                order.forEach(e => {
                  document.querySelectorAll(`[data-postid="${postId}"][data-kind="${e}"]`).forEach(function(el){
                    el.innerHTML = `${e} ${(counts[e]||0)}`
                  })
                })
              }

              async function reactPost(postId, kind){
                const prev = localStorage.getItem('reaction:' + postId) || ''
                const body = new URLSearchParams()
                body.append('kind', kind)
                if (prev) body.append('prev', prev)
                const tokenMeta = document.querySelector('meta[name="csrf-token"]')
                const token = tokenMeta ? tokenMeta.getAttribute('content') : ''
                const res = await fetch(`/posts/${postId}/react`, {
                  method: 'POST',
                  headers: {
                    'Content-Type': 'application/x-www-form-urlencoded',
                    'X-CSRF-Token': token
                  },
                  body: body.toString()
                })
                if (!res.ok) return
                const json = await res.json()
                // update inline buttons
                updateSummary(postId, json)
                // toggle local selection: if same clicked twice remove selection
                const prevVal = localStorage.getItem('reaction:' + postId)
                if (prevVal === kind) localStorage.removeItem('reaction:' + postId)
                else localStorage.setItem('reaction:' + postId, kind)
              }

              // attach handlers
              document.addEventListener('DOMContentLoaded', function(){
                document.querySelectorAll('.emoji-button').forEach(function(el){
                  el.addEventListener('click', function(){ reactPost(el.dataset.postid, el.dataset.kind) })
                  // set initial inline counts from summary element if present
                  const postId = el.dataset.postid
                  const summaryEl = document.getElementById('summary-' + postId)
                  if (summaryEl && summaryEl.dataset && summaryEl.dataset.counts) {
                    try {
                      const counts = JSON.parse(summaryEl.dataset.counts)
                      const kind = el.dataset.kind
                      el.innerHTML = `${kind} ${(counts[kind]||0)}`
                    } catch(_) {}
                  }
                })
              })
            })();
        |}</script>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Posts" PostsAction
                ]

        

renderPost :: Post -> Html
renderPost post = 
    if post.userId == currentUserId then [hsx|
    <tr>
        <!-- main column -->
        <td class="align-top">
            <div>
                <small class="text-muted">User {post.postUsername}</small>
            </div>
            <div class="mt-1">
                <h3 class="mb-1">
                    <a class="text-dark text-decoration-none" href={ShowPostAction post.id}>
                        {post.title}
                    </a>
                </h3>
                <p class="text-dark mb-3">{post.body}</p>
            </div>

            <!-- Reaction UI: show emojis inline (no React button) -->
            <div id={"reactions-" <> tshow post.id} class="mt-2">
                <div>
                    <span class="emoji me-2" onclick="chooseReaction('{post.id}','❤️')">❤️</span>
                    <span class="emoji me-2" onclick="chooseReaction('{post.id}','👍')">👍</span>
                    <span class="emoji me-2" onclick="chooseReaction('{post.id}','👎')">👎</span>
                    <span class="emoji me-2" onclick="chooseReaction('{post.id}','😂')">😂</span>
                    <span class="emoji me-2" onclick="chooseReaction('{post.id}','😍')">😍</span>
                    <span class="emoji me-2" onclick="chooseReaction('{post.id}','😢')">😢</span>
                    <span class="emoji me-2" onclick="chooseReaction('{post.id}','😱')">😱</span>
                    <span class="emoji me-2" onclick="chooseReaction('{post.id}','😡')">😡</span>
                </div>

                <div id={"summary-" <> tshow post.id} class="mt-2">
                    <!-- will be filled by JS -->
                    <small class="text-muted">Loading…</small>
                </div>
            </div>
    
        </td>

        <!-- right-side actions column -->
        <td class="align-top text-end" style="width:10%;">
                <div class="d-flex flex-column align-items-end">
                    <p class="text-muted text-decoration-none">{post.createdAt |> timeAgo}</p>

                    <a href={EditPostAction post.id} class="btn btn-sm btn-outline-secondary mb-2">Edit</a>
                    <a href={DeletePostAction post.id} class="btn btn-sm btn-outline-danger js-delete">Delete</a>
                </div>
        </td>

        
    </tr>
|] else [hsx|
    <tr>
        <!-- main column -->
        <td class="align-top">
            <div>
                <small class="text-muted">User {post.postUsername}</small>
            </div>
            <div class="mt-1">
                <h3 class="mb-1">
                    <a class="text-dark text-decoration-none" href={ShowPostAction post.id}>
                        {post.title}
                    </a>
                </h3>
                <p class="text-dark mb-3">{post.body}</p>
            </div>

            <!-- Reaction UI: show emojis inline (no React button) -->
            <div id={"reactions-" <> tshow post.id} class="mt-2">
                <div>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="❤️">❤️ {get #heartCount post}</span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="👍">👍 {get #plusCount post}</span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="👎">👎 {get #minusCount post}</span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="😂">😂 {get #laughCount post}</span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="😍">😍 {get #loveCount post}</span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="😢">😢 {get #cryCount post}</span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="😱">😱 {get #shockCount post}</span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="😡">😡 {get #angryCount post}</span>
                </div>

            </div>
        </td>

                <!-- right-side actions column -->
        <td class="align-top text-end" style="width:20%;">
                <div class="d-flex flex-column align-items-end">
                    <p class="text-muted text-decoration-none">{post.createdAt |> timeAgo}</p>
                </div>
        </td>

    </tr>
|]


