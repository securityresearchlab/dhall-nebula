package main

import (
	"github.com/gin-gonic/gin"
	// "io/ioutil"
	"net/http"
)

func main() {
	r := gin.Default()
	r.SetTrustedProxies(nil)
	r.POST("/:clientname", func(c *gin.Context) {
		var name = c.Param("clientname")
		formFile, _ := c.FormFile("file")
		err := c.SaveUploadedFile(formFile, name + ".txt")
		if err != nil {
			c.Status(http.StatusInternalServerError)
		} else {
			c.Status(http.StatusOK)
		}
		return
	})
	r.Run() // listen and serve on 0.0.0.0:8080 (for windows "localhost:8080")
}
