const { animate, onScroll, splitText } = anime;

const title = document.querySelector("#page-title")

const slides = document.querySelectorAll(".slide");

slides.forEach((slide) => {
    const contents = slide.querySelector(".slide-contents");
    animate(contents, {
        transform: ["scale(2)", "scale(1)"],
        opacity: [0, 100],
        filter: ["blur(0.5rem)",  "blur(0rem)"],
        easing: 'easeInOutSine',
        autoplay: onScroll({
            target: contents,
            leave: 'bottom',
            sync: 0.8,
            alternate: true,
        })
    })
});


