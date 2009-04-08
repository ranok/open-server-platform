#define FUSE_USE_VERSION 26

#include <fuse.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>

#include <erl_interface.h>
#include <ei.h>

static const char *erlfs_str = "For the win!\n";
static const char *erlfs_path = "/afile";

static int erlfs_getattr(const char *path, struct stat *stbuf)
{
	int res = 0;

	memset(stbuf, 0, sizeof(struct stat));
	if (strcmp(path, "/") == 0) {
		stbuf->st_mode = S_IFDIR | 0755;
		stbuf->st_nlink = 2;
	} else if (strcmp(path, erlfs_path) == 0) {
		stbuf->st_mode = S_IFREG | 0444;
		stbuf->st_nlink = 1;
		stbuf->st_size = strlen(erlfs_str);
	} else
		res = -ENOENT;

	return res;
}

static int erlfs_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
			 off_t offset, struct fuse_file_info *fi)
{
	(void) offset;
	(void) fi;

	if (strcmp(path, "/") != 0)
		return -ENOENT;

	filler(buf, ".", NULL, 0);
	filler(buf, "..", NULL, 0);
	filler(buf, erlfs_path + 1, NULL, 0);

	return 0;
}

static int erlfs_open(const char *path, struct fuse_file_info *fi)
{
	if (strcmp(path, erlfs_path) != 0)
		return -ENOENT;

	if ((fi->flags & 3) != O_RDONLY)
		return -EACCES;

	return 0;
}

static int erlfs_read(const char *path, char *buf, size_t size, off_t offset,
		      struct fuse_file_info *fi)
{
	size_t len;
	(void) fi;
	if(strcmp(path, erlfs_path) != 0)
		return -ENOENT;

	len = strlen(erlfs_str);
	if (offset < len) {
		if (offset + size > len)
			size = len - offset;
		memcpy(buf, erlfs_str + offset, size);
	} else
		size = 0;

	return size;
}

static struct fuse_operations erlfs_oper = {
	.getattr	= erlfs_getattr,
	.readdir	= erlfs_readdir,
	.open		= erlfs_open,
	.read		= erlfs_read,
};

int main(int argc, char *argv[])
{
  printf("Cyrus was here!\n");
	return fuse_main(argc, argv, &erlfs_oper, NULL);
}

