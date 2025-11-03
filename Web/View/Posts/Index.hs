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

            <!-- Reaction UI -->
            <div id={"reactions-" <> tshow post.id} class="mt-2">
                <button type="button" class="btn btn-outline-primary btn-sm" onclick="togglePicker('{post.id}')">React</button>
                <div id={"picker-" <> tshow post.id} style="display:none; margin-top:.5rem;">
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

            <!-- Reaction UI -->
            <div id={"reactions-" <> tshow post.id} class="mt-2">
                <button type="button" class="btn btn-outline-primary btn-sm" onclick="togglePicker('{post.id}')">React</button>
                <div id={"picker-" <> tshow post.id} style="display:none; margin-top:.5rem;">
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
        <td class="align-top text-end" style="width:20%;">
                <div class="d-flex flex-column align-items-end">
                    <p class="text-muted text-decoration-none">{post.createdAt |> timeAgo}</p>
                </div>
        </td>

    </tr>
|]




-- reactionsScript :: Post -> Html
--         reactionsScript = [hsx|
--           <script>{raw|
--             (function(){
--               function storageKeyCounts(postId){ return 'post:counts:' + postId }
--               function storageKeyYour(postId){ return 'post:your:' + postId }

--               function initCounts(postId) {
--                 const key = storageKeyCounts(postId)
--                 let counts = JSON.parse(localStorage.getItem(key) || '{}')
--                 const order = ['❤️','👍','👎','😂','😍','😢','😱','😡']
--                 if (!counts || Object.keys(counts).length === 0) {
--                   counts = {}; order.forEach(e => counts[e] = 0)
--                   localStorage.setItem(key, JSON.stringify(counts))
--                 }
--                 renderSummary(postId, counts)
--               }

--               window.togglePicker = function(postId){
--                 const el = document.getElementById('picker-' + postId)
--                 if (!el) return
--                 el.style.display = el.style.display === 'none' ? 'block' : 'none'
--               }

--               window.chooseReaction = function(postId, emoji){
--                 const yourKey = storageKeyYour(postId)
--                 const countsKey = storageKeyCounts(postId)

--                 const prev = localStorage.getItem(yourKey)
--                 let counts = JSON.parse(localStorage.getItem(countsKey) || '{}')

--                 if (prev && prev !== emoji) {
--                   counts[prev] = Math.max(0, (counts[prev] || 0) - 1)
--                 }

--                 if (prev === emoji) {
--                   localStorage.removeItem(yourKey)
--                 } else {
--                   counts[emoji] = (counts[emoji] || 0) + 1
--                   localStorage.setItem(yourKey, emoji)
--                 }

--                 localStorage.setItem(countsKey, JSON.stringify(counts))
--                 renderSummary(postId, counts)
--                 togglePicker(postId)
--               }

--               function renderSummary(postId, counts){
--                 const s = document.getElementById('summary-' + postId)
--                 if (!s) return
--                 const order = ['❤️','👍','👎','😂','😍','😢','😱','😡']
--                 let html = '<small class="text-muted">'
--                 html += order.map(e => `${e} ${(counts[e]||0)}`).join(' &nbsp; ')
--                 html += '</small>'
--                 s.innerHTML = html
--               }

--               document.addEventListener('DOMContentLoaded', function(){
--                 document.querySelectorAll('[id^="reactions-"]').forEach(function(el){
--                   const postId = el.id.replace('reactions-','')
--                   initCounts(postId)
--                 })
--               })
--             })();
--           |}</script>
--         |]