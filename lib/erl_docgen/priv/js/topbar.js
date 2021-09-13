
function toggleDisplay() {
    var leftnav=document.getElementById('leftnav');
    var topbar=document.getElementsByClassName('topbar-expand')[0];
    if (leftnav.classList.contains('show')) {
        leftnav.classList.remove('show');
        topbar.classList.remove('show');
        setTimeout(() => {
            leftnav.classList.add('hide-mobile');
        }, 350);
    } else {
        leftnav.classList.add('show');
        topbar.classList.add('show');
        leftnav.classList.remove('hide-mobile');
    }
}
