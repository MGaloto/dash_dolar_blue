
LRED=$(printf '\033[1;31m')
LGREEN=$(printf '\033[1;32m')

echo "..........Rendering the dashboard..............."
if [[ "$1" = ""  || "$2" = "" ]] ; then
    echo "${LRED}The git user.name and/or user.email are missing${LRED}"
    exit 0
else
    echo "${LGREEN}Git user.name is: $1${LGREEN}"
    echo "${LGREEN}Git user.email is: $2${LGREEN}"
fi

Rscript etl.R
echo "${LGREEN}----------Ok Rscript---------${LGREEN}"

# Fix github issue
git config --global --add safe.directory /__w/dash_dolar_blue/dash_dolar_blue

changes=$(git status --porcelain)

if [[ "$changes" != "" ]]; then
    git config --global user.name $1
    git config --global user.email $2
    git add *
    git commit -m "Auto update dashboard"
    git pull
    git push
    echo "${LGREEN}Se registraron cambios en el repositorio.${LGREEN}"
    echo "${LGREEN}Archivos modificados:${LGREEN}"
    echo "$changes"
else
    echo "OK Auto update dashboard. ${LRED}No${LRED} se registraron cambios en el ${LGREEN}repositorio${LGREEN}."
fi
