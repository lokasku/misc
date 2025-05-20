/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./app/*.hs", "./app/Article/*.hs", "./app/Route/*.hs"],
  theme: {
    fontFamily: {
      'sans': ['PP Neue Montreal', 'Helvetica', 'sans-serif'],
      'mono': ['monospace']
    },
    extend: {}
  },
  plugins: [],
}